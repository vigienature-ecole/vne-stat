#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(magrittr)

app_config = data.table::fread("data/app_config.csv")
app_config <- app_config[order(app_config$label), ]

protocole_names <- app_config[type == "data"]$valeur
data_values <- purrr::map(paste0("../../datasets/papers/", protocole_names, ".csv"), data.table::fread)
names(data_values) <- protocole_names

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # app starts ----
  
  # init module
  # mod_login_server("login", parent_session = session)
  # mod_network_server("reseaux", parent_session = session)
  
  # hide elements when app starts
  hide("start_data")
  hide("manipulate")
  hide("variable_group")
  hide("variable_filter")
  hide("variable_level")
  hide("species_filter")
  hide("species_filter")
  hide("start_manip")
  hide("visualize")
  hide("start_visu")
  
  # app_values
  app_values <- reactiveValues(
    open_panel = NULL,
    start_analysis = FALSE,
    return_to_input = FALSE,
    current_dataset_name = NULL,
    current_dataset = NULL,
    result_manip_view = NULL
  )
  
  # populate select input import
  updateSelectInput(session, "import", choices = c("Choisir un jeu de données", 
                                                   setNames(app_config$valeur[app_config$type == "data"], 
                                                            app_config$label[app_config$type == "data"])))
  
  # when dataset is selected : ----
  # show button to access to raw data
  # show selection for data manipulation
  # populate manipulation possibilities
  # import and store dataset
  
  observeEvent(input$import, {
    cat("Choose dataset\n")
    
    if(input$import != "Choisir un jeu de données"){ 
      show("start_data")
      show("manipulate")
      current_dataset_name = input$import
      app_values$current_dataset <- data_values[[input$import]]
      dataset_info <- app_config[type == "operation" & app_config[[current_dataset_name]]]
      updateSelectInput(session, 
                        "manipulate", 
                        choices = c("Choisir une opération", setNames(dataset_info$valeur, 
                                                                      dataset_info$label)))
      
    } else {
      hide("start_data")
      hide("manipulate")
    }
  })
  
  # when manipulation is selected ----
  # show button to access to result (table)
  # show options (group, filter)
  # calculate on click
  observeEvent(input$manipulate, {
    cat("Choose operation :\n")
    
    if(input$manipulate != "Choisir une opération" & input$manipulate != "reseau"){ 
      show("visualize")
      current_dataset_name = input$import
      visualisation_information <- app_config[app_config$type == "visualisation" & app_config[[current_dataset_name]]]
      
      updateSelectInput(session, "visualize", choices = c("Choisir un type de visualisation", setNames(visualisation_information$valeur, 
                                                                                                       visualisation_information$label)))
      show("start_manip")
    } else {
      hide("visualize")
      hide("start_manip")
    }
    
    if(input$manipulate %in% c("diversite", "nombre_obs", "abondance")){ 
      show("variable_group")
      current_dataset_name = input$import
      variable_information <- app_config[app_config$type == "variable" & app_config[[current_dataset_name]]]
      
      updateSelectInput(session, "variable_group", choices = c("Choisir une variable", setNames(variable_information$valeur, 
                                                                                                variable_information$label)))
    } else {
      hide("variable_group")
    }
    
    if(input$manipulate == "abondance"){
      show("species_filter")
      updateSelectInput(session, "species_filter", choices = c("Choisir une variable", unique(app_values$current_dataset$Espece)))
    } else {
      hide("species_filter")
    }
    
    if(input$manipulate == "nombre_especes"){
      show("variable_filter")
      updateSelectInput(session, "variable_filter", choices = c("Choisir une variable", sort(colnames(app_values$current_dataset))))
    } else {
      hide("variable_filter")
    }
    
    if(input$manipulate == "reseau") {
      show("start_visu")
      output$help_manip <- renderText("Le résultat de l'outil de visualisation de réseaux n'est disponible que dans l'onglet visualisation")
    }
  })
  
  observeEvent(input$variable_filter, {
    if(input$variable_filter != "Choisir une variable"){
      if(input$variable_filter %in% unique(app_values$current_dataset[,..input$variable_filter])){
        show("variable_level")
        output$help_level <- renderText("Maintenant que vous avez choisi la variable à utiliser, il vous faut choisir la valeur de cette variable pour laquelle le tri sera effectué")
        updateSelectInput(session, "variable_level", choices = c("Choisir une variable", unique(app_values$current_dataset[[input$variable_filter]])))
      } else {
        hide("variable_level")
        output$help_level <- NULL
      }
    }
  })
  
  # when visualisation is selected ----
  # show button to access to result (visu)
  # visualise on click
  observeEvent(input$visualize, {
    cat("Choose visualisation\n")
    if(input$visualize != "Choisir un type de visualisation"){ 
      show("start_visu")
    } else {
      hide("start_visu")
    }
  })
  
  # click buttons ----
  
  observeEvent(input$start_data, {
    cat("View raw data\n")
    app_values$open_panel <- "Données importées"
    app_values$start_analysis <- TRUE
  })
  
  observeEvent(input$start_manip, {
    cat("View manip result\n")
    app_values$open_panel <- "Manipulation"
    app_values$start_analysis <- TRUE
  })
  
  observeEvent(input$start_visu, {
    cat("View viz result\n")
    if(input$manipulate == "reseau"){
      updateTabsetPanel(session, "vne_stats",
                        selected = "reseau")
    } else {
      app_values$start_analysis <- TRUE
      app_values$open_panel <- "Visualisation"
    }
  })
  
  observeEvent(app_values$start_analysis, {
    if(app_values$start_analysis){
      cat("Start analysis\n")
      
      # navigation
      updateTabsetPanel(session, "vne_stats",
                        selected = "results")
      
      updateCollapse(session, id = "results", open = app_values$open_panel)
      
      # raw data
      app_values$current_dataset <- data_values[[input$import]]
      
      if(input$manipulate == "abondance" | input$manipulate == "diversite" | input$manipulate == "nombre_obs"){
        results_manip <- calculate_indices(data = app_values$current_dataset, index = input$manipulate, variable_group = input$variable_group)
        
        app_values$result_manip <- results_manip[["result_manip"]]
        app_values$result_manip_view <- results_manip[["view"]]
        
      } else if(input$manipulate == "nombre_especes"){
        
      } else if(input$manipulate == "reseau"){
        
      } else {
        app_values$result_manip_view <- NULL
        output$error_manip <- renderText("Vous n'avez pas encore sélectionné d'opération à effectuer. Vous pouvez utiliser le menu déroulant de l'étape 2 pour en choisir une.")
      }
      
      # mettre ailleur ?
      output$manip_output <-  DT::renderDataTable(app_values$result_manip_view, 
                                                  options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE)
      
      # Visualisation ----
      if(input$visualize == "graphique") {
        graph <- make_graph(
          data_to_plot = app_values$result_manip,
          variable_group = input$variable_group,
          variable_info = app_config[valeur == input$variable_group],
          current_dataset_name = input$import,
          index_type = input$manipulate)
        output$visu_graph_output <- renderPlot(graph)
      }
    }
  })
  
  # buttons to return to the input interface
  observeEvent(input$new_analysis_bottom, {
    app_values$return_to_input <- TRUE
  })
  
  observeEvent(input$new_analysis_top_network, {
    app_values$return_to_input <- TRUE
  })
  
  
  observeEvent(input$new_analysis_top, {
    app_values$return_to_input <- TRUE
  })
  
  observeEvent(app_values$return_to_input, {
    if(app_values$return_to_input){
      cat("return to ui\n")
      updateTabsetPanel(session, 
                        "vne_stats",
                        selected = "input")
      app_values$start_analysis <- FALSE
      app_values$return_to_input <- FALSE
    }
  })
  
  # Results
  output$data_output <- DT::renderDataTable(
    app_values$current_dataset, 
    options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE
  )
}
