# load packages
library(shiny)
library(shinyjs)

# load the app_config file (datasets, operation and variable to be used within the application)
app_config = data.table::fread("data/app_config.csv", encoding = "UTF-8")
app_config <- app_config[order(app_config$label), ]

# get the protocol names and load them
protocole_names <- app_config[type == "data"]$valeur
data_values <- purrr::map(paste0("../../datasets/papers/", protocole_names, ".csv"), data.table::fread)
names(data_values) <- protocole_names

# Define server logic
function(input, output, session) {
  
  # app starts ----
  
  # init all modules
  mod_login_server("login", parent_session = session)
  mod_network_server("reseaux", parent_session = session)
  mod_network_biolit_server("reseaux_biolit", parent_session = session)
  mod_map_birds_server("map_birds", parent_session = session)
  mod_habitats_sauvages_server("habitats_sauvages", parent_session = session, data_values = data_values)
  mod_study_plant_spipoll_server("study_plant_spipoll", parent_session = session, data_values = data_values)
  # mod_map_insects_server("map_insects", parent_session = session)
  
  # hide elements when app starts
  hide("view_raw_data")
  hide("manipulate")
  hide("variable_group")
  hide("variable_filter")
  hide("variable_level")
  hide("species_filter")
  hide("view_res_manip")
  hide("visualize")
  hide("view_res_visu")
  
  # define app_values
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
      show("view_raw_data")
      show("manipulate")
      # get name of current dataset
      current_dataset_name = input$import
      # get information on the dataset
      app_values$current_dataset <- data_values[[input$import]]
      dataset_info <- app_config[type == "operation" & app_config[[current_dataset_name]]]
      # populate with the operation that are related to the current dataset
      updateSelectInput(session, 
                        "manipulate", 
                        choices = c("Choisir une opération", setNames(dataset_info$valeur, 
                                                                      dataset_info$label)))
      
    } else {
      # if the dataset is not selected, hide all
      hide("view_raw_data")
      hide("manipulate")
      hide("variable_filter")
      hide("variable_filter")
      hide("variable_group")
      hide("view_res_manip")
      hide("view_res_visu")
    }
  })
  
  # make sure variable filter is hidden
  hide("variable_filter")
  # when manipulation is selected ----
  # show button to access to result (table)
  # show options (group, filter)
  # calculate on click
  observeEvent(input$manipulate, {
    cat("Choose operation :\n")
    if (input$manipulate != "Choisir une opération" ){
      show("view_res_visu")
    } else {
      hide("view_res_visu")
    }
    
    
    if(input$manipulate %in% c("Choisir une opération","reseau", "map_birds", "maps_insects", "sauvages_habitats")){ 
      hide("view_res_manip")
    } else {
      show("view_res_manip")
    }
    
    if(input$manipulate %in% c("diversite", "nombre_obs", "abondance", "activity")){ 
      
      show("variable_group")
      current_dataset_name = input$import
      variable_information <- app_config[app_config$type == "variable" & app_config[[current_dataset_name]]]
      
      updateSelectInput(session, "variable_group", choices = c("Choisir une variable", setNames(variable_information$valeur, 
                                                                                                variable_information$label)))
    } else {
      hide("variable_group")
    }
    
    if(input$manipulate == "abondance" | input$manipulate == "activity"){
      show("species_filter")
      updateSelectInput(session, "species_filter", choices = c("Choisir une espèce", sort(unique(app_values$current_dataset$Espece))))
    } else {
      hide("species_filter")
    }
    
    if(input$manipulate %in% c("diversite", "nombre_obs", "abondance", "nombre_especes", "activity")){
      show("variable_filter")
      updateSelectInput(session, "variable_filter", choices = c("Choisir une variable", sort(colnames(app_values$current_dataset)[!colnames(app_values$current_dataset) %in% c("Numero_observation", "Espece", "Nombre_individus")])))
    } else {
      hide("variable_filter")
    }
    
    if(input$manipulate == "reseau") {
      output$help_manip <- renderText("Le résultat de l'outil de visualisation de réseaux n'est disponible que dans l'onglet visualisation")
    } else if (input$manipulate == "map_birds" | input$manipulate == "map_insects" ){
      output$help_manip <- renderText("Le résultat de l'outil de visualisation de carte n'est disponible que dans l'onglet visualisation")
    } else {
      output$help_manip <- NULL
    }
  })
  
  observeEvent(input$variable_filter, {
    cat("filter with variable")
    if(input$variable_filter != "Choisir une variable"){
      cat(" active")
      if(input$variable_filter %in% unique(app_values$current_dataset[,..input$variable_filter])){
        show("variable_level")
        output$help_level <- renderText("Maintenant que vous avez choisi la variable à utiliser, il vous faut choisir la valeur de cette variable pour laquelle le tri sera effectué")
        updateSelectInput(session, "variable_level", choices = c("Choisir une valeur", unique(app_values$current_dataset[[input$variable_filter]])))
      } else {
        hide("variable_level")
        output$help_level <- NULL
        shinyjs::reset("variable_level")
      }
    } else {
      cat(" inactive")
      hide("variable_level")
      output$help_level <- NULL
      shinyjs::reset("variable_level")
    }
  })
  
  # click buttons ----
  
  observeEvent(input$view_raw_data, {
    cat("View raw data\n")
    app_values$open_panel <- "Données importées"
    app_values$start_analysis <- TRUE
  })
  
  observeEvent(input$view_res_manip, {
    cat("View manip result\n")
    app_values$open_panel <- "Manipulation"
    app_values$start_analysis <- TRUE
  })
  
  observeEvent(input$view_res_visu, {
    cat("View viz result\n")
    if(input$manipulate == "reseau") {
      if(input$import == "biolit") {
        updateTabsetPanel(session, "vne_stats",
                          selected = "reseau_biolit")
      } else {
        updateTabsetPanel(session, "vne_stats",
                          selected = "reseau")
      }
      
    } else if(input$manipulate == "sauvages_habitats") {
      updateTabsetPanel(session, "vne_stats",
                        selected = "sauvages_habitats")
    } else if(input$manipulate == "map_birds") {
      updateTabsetPanel(session, "vne_stats",
                        selected = "map_birds")
    } else if(input$manipulate == "analyse_plant") {
      updateTabsetPanel(session, "vne_stats",
                        selected = "study_plant_spipoll")
      
    } else if(input$manipulate == "map_insects") {
      updateTabsetPanel(session, "vne_stats",
                        selected = "map_insects")
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
      # remove NA from Nombre_individus
      
      
      if(input$import == "escargots") {
        app_values$current_dataset <- app_values$current_dataset[!is.na(Nombre_individus) ]
      }
      
      # data wrandling
      if (input$variable_filter != "Choisir une variable"){
        if(input$variable_level != "Choisir une valeur"){
          values_to_filter <- app_values$current_dataset[[input[["variable_filter"]]]] == input$variable_level
          current_dataset <- app_values$current_dataset[values_to_filter, ]
        } else {
          # add error !!!!!
          current_dataset <- app_values$current_dataset
          output$filter_error <- "Attention le filtre n'a pas été mis en place car aucune valeur de catégorie n'a été sélectionnée"
        }
      } else {
        current_dataset <- app_values$current_dataset
      }
      
      # filter dataset
      
      if(input$manipulate == "abondance" | input$manipulate == "diversite" | input$manipulate == "nombre_obs" | input$manipulate == "activity"){
        
        
        if ((input$manipulate == "activity" | input$manipulate == "abondance") & input$species_filter != "Choisir une espèce"){
          current_dataset <- current_dataset[Espece == input$species_filter]
        } else {
          current_dataset <- current_dataset
        }
        
        results_manip <- calculate_indices(data = current_dataset, index = input$manipulate, variable_group = input$variable_group)
        
        app_values$result_manip <- results_manip[["result_manip"]]
        app_values$result_manip_view <- results_manip[["view"]]
        
      } else if(input$manipulate == "nombre_especes"){
        
        if (input$import == "sauvages"){
          current_dataset$Nombre_individus <- 1
        } else if(input$import == "chiro"){
          current_dataset$Nombre_individus <- current_dataset$Nombre_contacts
        }
        
        results_manip = current_dataset[ , .(index = length(Nombre_individus[Nombre_individus > 0])),
                                         by = Espece]
        
        app_values$result_manip <- results_manip
        app_values$result_manip_view <- results_manip
        
      } else if(input$manipulate == "reseau"){
        
      } else {
        app_values$result_manip_view <- NULL
      }
      
      # mettre ailleur ?
      output$manip_output <-  DT::renderDataTable(app_values$result_manip_view, 
                                                  options = list(pageLength = 24, dom = 'tp', searching = FALSE), escape = FALSE)
      
      # Visualisation ----
      
      if (!is.null(app_values$result_manip)){
        graph <- make_graph(
          data_to_plot = app_values$result_manip,
          variable_group = input$variable_group,
          variable_info = app_config[valeur == input$variable_group],
          current_dataset_name = input$import,
          index_type = input$manipulate)
        output$visu_graph_output <- renderPlot({graph}, height = 500)
      }
      
    } else {
      cat("\nanalysis waiting")
    }
  })
  
  output$title_graph <- renderText({
    if(input$manipulate == "abondance" | input$manipulate == "diversite" | input$manipulate == "nombre_obs" | input$manipulate == "activity"){
      title_graph <- app_config$label[app_config$valeur == input$manipulate]
      
      if (input$variable_group != "Choisir une variable") {
        title_graph <- paste(title_graph,
                             "en fonction de la variable",
                             app_config$label[app_config$valeur == input$variable_group])
      }
      
      if ((input$manipulate == "activity" | input$manipulate == "abondance") & input$species_filter != "Choisir une espèce") {
        title_graph <- paste(title_graph,
                             "pour l'espèce",
                             input$species_filter)
      }
      
      if (input$variable_filter != "Choisir une variable") {
        title_graph <- paste(title_graph,
                             "pour la valeur",
                             input$variable_level,
                             "de la variable",
                             input$variable_filter)
      }
      
      title_graph
    } else {
      NULL
    }
  })
  
  # buttons to return to the input interface
  observeEvent(input$new_analysis_bottom, {
    app_values$return_to_input <- TRUE
  })
  
  
  
  
  observeEvent(input$new_analysis_top, {
    app_values$return_to_input <- TRUE
  })
  observeEvent(input$new_analysis_top_sauvages_habitats, {
    app_values$return_to_input <- TRUE
  })
  
  
  observeEvent(input$new_analysis_top_network, {
    app_values$return_to_input <- TRUE
  })
  
  observeEvent(input$new_analysis_top_map_birds, {
    app_values$return_to_input <- TRUE
  })
  
  
  # observeEvent(input$new_analysis_top_map_insects, {
  #   app_values$return_to_input <- TRUE
  # })
  
  observeEvent(input$new_analysis_top_network_biolit, {
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
