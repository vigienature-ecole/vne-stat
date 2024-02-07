mod_habitats_sauvages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
      h3("Paramètres du graphique"),
      selectInput(ns("espece"), "Choix de l'espèce", choices = c("Choisir une espèce de plante"))
    ),
    
    
    
    
    column(width = 8,
           htmlOutput(ns("title")) %>% 
             tagAppendAttributes(style = 'color:#62CC33;font-weight: bolder;font-size: x-large;'),
           uiOutput(ns("plant_image")),
           div(
             plotOutput(ns("plot_habitats"), height = '600px' )
           )
           
    )
  )
}


mod_habitats_sauvages_server <- function(id, parent_session, data_values){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Variables for module
    
    mod_values <- reactiveValues(
      
      
    )
    # Create a reactiveValues object to store the flag
    values <- reactiveValues(selectUpdated = FALSE)
    
    # Observe the module being loaded and update selectInput once
    observe({
      if (!values$selectUpdated) {
        # Update choices in selectInput
        
        liste_especes <- sort(unique(data_values$sauvages$Espece))
        # remove espece indeterminees
        liste_especes <- liste_especes[!grepl("Espèce non déterminée", liste_especes)]
        
        
        updateSelectInput(session, "espece", choices = c("Choisir une espèce de plante", liste_especes))
        
        # Set the flag to indicate that the update has been done
        values$selectUpdated <- TRUE
      }
    })
    
    output$plot_habitats <- renderPlot({
      if(input$espece != "Choisir une espèce de plante"){
        # browser()
        library(ggplot2)
        data_values$sauvages$Chemin <-  ifelse(data_values$sauvages$Chemin == "oui", 1, 0)
        data_values$sauvages$Fissure <-  ifelse(data_values$sauvages$Fissure == "oui", 1, 0)
        data_values$sauvages$Haie <-  ifelse(data_values$sauvages$Haie == "oui", 1, 0)
        data_values$sauvages$Mur <-  ifelse(data_values$sauvages$Mur == "oui", 1, 0)
        data_values$sauvages$Pelouse <-  ifelse(data_values$sauvages$Pelouse == "oui", 1, 0)
        data_values$sauvages$Pied_d_arbre <-  ifelse(data_values$sauvages$Pied_d_arbre == "oui", 1, 0)
        data_values$sauvages$Platebande <-  ifelse(data_values$sauvages$Platebande == "oui", 1, 0)
        
        columns_to_keep <- c("Espece", "Chemin", "Fissure", "Haie", "Mur", "Pelouse", "Pied_d_arbre", "Platebande")
        
        focal_species <- input$espece
        
        longer <- data_values$sauvages[, ..columns_to_keep] %>%
          tidyr::pivot_longer(cols = 2:8, names_to = "Habitat") 
        
        
        
        
        resultats_habitats <- longer|>
          dplyr::group_by(Habitat)|>
          dplyr::summarise(Nombre_observation_hab = sum(value))
        
        resultats_habitats_species <- longer |>
          dplyr::filter(Espece == focal_species) |>
          dplyr::group_by(Espece, Habitat)|>
          dplyr::summarise(Nombre_observation = sum(value))
        
        resultats_globaux <- dplyr::inner_join(resultats_habitats_species, resultats_habitats, by = "Habitat")
        
        resultats_globaux$Proportion_observation_corrige <- resultats_globaux$Nombre_observation / resultats_globaux$Nombre_observation_hab
        
        
        theme =   ggplot2::theme_bw() +
          ggplot2::theme(axis.text.y=element_text(size = 20),
                         axis.text.x=element_text(size = 20, angle = 30, hjust = 1),
                         axis.title=element_text(size = 24),
                         strip.text.x = element_text(size = 20),
                         axis.title.x = element_text(vjust = -2),
                         axis.title.y = element_text(vjust = 2.3),
                         plot.margin=unit(c(1,1,1.5,1.2),"cm"),
                         legend.position = "none",
                         plot.caption = element_text(size = 16))
        
        
        plot1 <- ggplot2::ggplot(resultats_globaux, ggplot2::aes( x = Habitat, y = Proportion_observation_corrige)) +
          ggplot2::geom_col() +
          ggplot2::labs(y = "Proportion d'observations de\n l'espèce dans l'habitat") +
          theme
        
        plot3 <- ggplot2::ggplot(resultats_globaux, ggplot2::aes( x = Habitat, y = Nombre_observation)) +
          ggplot2::geom_col() +
          ggplot2::labs(y = "Nombre d'observations de\n l'espèce dans l'habitat") +
          theme
        
        
        plot2 <- ggplot2::ggplot(resultats_habitats, ggplot2::aes( x = Habitat, y = Nombre_observation_hab)) +
          ggplot2::geom_col() +
          ggplot2::labs(y = "Nombre d'observations \nde l'habitat") +
          theme
          
        plot1
        
        
        
      } else {
        NULL
      }
    },height = 600)
    
    
    
    output$title <- renderText({
      if (input$espece == "Choisir une espèce de plante"){
        ""
      } else {
        paste("Types d'habitats de l'espèce : ", input$espece)
      }
    })
    
    output$bird_image <- renderUI({
      if (input$variable == "total_observation"){
        NULL
      } else {
        tags$img(src = paste0("https://depot.vigienature-ecole.fr/restits/Bilan/2020/img/vignettes_oiseaux/", 
                              stringr::str_to_title(
                                iconv(
                                  gsub("-", "_", 
                                       gsub("'", "_", 
                                            gsub(" ", "_", input$espece_focale)))
                                  ,from="UTF-8",to="ASCII//TRANSLIT")
                              )
                              ,"_1.jpg")
                 
                 , height = "150px"
        )
      }
    })
    
    
    
  })
}


