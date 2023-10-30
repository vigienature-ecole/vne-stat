mod_map_birds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
      h3("Paramètres de la carte"),
      selectInput(ns("map_type"), "Choix du type de carte", choices = setNames(c("carte_grille_10",
                                                                                 "carte_grille_20",
                                                                                 "carte_grille_30",
                                                                                 "carte_grille_INPN",
                                                                                 "carte_academie",
                                                                                 "carte_departement",
                                                                                 "carte_region",
                                                                                 "carte_france"),
                                                                               c("Grille de 10x10",
                                                                                 "Grille de 20x20",
                                                                                 "Grille de 30x30",
                                                                                 "Grille INPN (10kmx10km)",
                                                                                 "Carte par academie",
                                                                                 "Carte par departement",
                                                                                 "Carte par region",
                                                                                 "Carte de la france"))),
      selectInput(ns("variable"), "Choix de la variable à représenter", choices = setNames(c(
        "frequence_observation",
        "total_observation",
        "total_observation_"
      ), c(
        "Frequence d'observation de l'espèce",
        "Total des observations",
        "Total des observations d'espèces"
      )
      )),
      selectInput(ns("period"), "Choix de la période à représenter", choices = setNames(c(
        "all",
        "mois",
        "saison"), c(
          "Toute l'année",
          "Par mois",
          "Par saison")
      )),
      
      
      selectInput(ns("espece_focale"), "Choix de l'espèce à représenterr", choices = sort(c("Accenteur mouchet", 
                                                                                            "Bergeronnette grise",
                                                                                            "Bouvreuil pivoine",
                                                                                            "Bruant jaune",
                                                                                            "Bruant zizi", 
                                                                                            "Chardonneret élégant", 
                                                                                            "Choucas des tours",  
                                                                                            "Corneille noire",
                                                                                            "Épervier d'Europe",
                                                                                            "Étourneau sansonnet",
                                                                                            "Fauvette à tête noire", 
                                                                                            "Geai des chênes", 
                                                                                            "Gobemouche gris",
                                                                                            "Grimpereau des jardins",
                                                                                            "Grive draine",
                                                                                            "Grive mauvis",
                                                                                            "Grive musicienne", 
                                                                                            "Grosbec casse-noyaux",
                                                                                            "Hirondelle de fenêtre", 
                                                                                            "Hirondelle rustique",
                                                                                            "Huppe fasciée",
                                                                                            "Linotte mélodieuse", 
                                                                                            "Martinet noir",
                                                                                            "Merle noir",
                                                                                            "Mésange à longue queue", 
                                                                                            "Mésange bleue", 
                                                                                            "Mésange charbonnière", 
                                                                                            "Mésange huppée", 
                                                                                            "Mésange noire",
                                                                                            "Mésange nonnette", 
                                                                                            "Moineau domestique", 
                                                                                            "Moineau friquet", 
                                                                                            "Perruche à collier",
                                                                                            "Pic épeiche", 
                                                                                            "Pic épeichette",
                                                                                            "Pic vert",
                                                                                            "Pie bavarde",
                                                                                            "Pigeon biset domestique", 
                                                                                            "Pigeon colombin", 
                                                                                            "Pigeon ramier",
                                                                                            "Pinson des arbres",
                                                                                            "Pinson du Nord", 
                                                                                            "Pouillot véloce",
                                                                                            "Roitelet huppé", 
                                                                                            "Rougegorge familier", 
                                                                                            "Rougequeue à front blanc",
                                                                                            "Rougequeue noir",
                                                                                            "Serin cini",
                                                                                            "Sittelle torchepot",
                                                                                            "Tarin des aulnes",
                                                                                            "Tourterelle turque", 
                                                                                            "Troglodyte mignon", 
                                                                                            "Verdier d'Europe"))),
      sliderInput(ns("min_obs"),
                  "Nombre minimum d'observation pour représentation",
                  min = 0,
                  max = 300,
                  value = 5)
    ),
    column( width = 8,
            uiOutput(ns("bird_image")),
            plotOutput(ns("map")),plotOutput(ns("graph"))
            
    )
  )
}


mod_map_birds_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Variables for module
    mod_values <- reactiveValues(
      maps = readRDS("../../datasets/papers/map_data_bird.rds"),
      carte_france = sf::read_sf("../../datasets/maps/metropole-version-simplifiee.geojson")
    )
    
    output$map <- renderPlot({
      # select map
      map_to_plot <- mod_values$maps[[input$map_type]][[input$period]]
      
      if (input$variable == "total_observation"){
        variable_to_plot = "total_observation"
        hide("espece_focale")
      } else if (input$variable == "total_observation_"){
        variable_to_plot = paste0("total_observation_", input$espece_focale)
        show("espece_focale")
      } else {
        variable_to_plot = paste0("frequence_observation_", input$espece_focale)
        show("espece_focale")
      }
      
      # valeur minimale à représenter
      map_to_plot <- map_to_plot |>
        dplyr::filter(total_observation > input$min_obs) |>
        dplyr::filter_at(dplyr::vars(variable_to_plot), dplyr::any_vars(. > 0))
      
      # carte à réaliser
      observation_map <- tmap::tm_shape(mod_values$carte_france) +  
        tmap::tm_borders()
      
      
      observation_map <-  observation_map + 
        tmap::tm_shape(map_to_plot) +  
        tmap::tm_fill(col = variable_to_plot, n = 10) 
      
      if(input$period != "all"){
        observation_map <- observation_map + tmap::tm_facets(by = input$period, free.coords = FALSE)
      } 
      
      observation_map <- observation_map +
        tmap::tm_shape(mod_values$carte_france) +  
        tmap::tm_borders() +
        tmap::tm_layout(frame = FALSE, legend.outside = TRUE, frame.lwd = NA, panel.label.bg.color = NA, panel.label.size = 2)
      
      observation_map
    })
    
    output$bird_image <- renderUI({
      
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
    })
    
    # output$graph <- renderPlot({
    # 
    #   # select map
    #   map_to_plot <- mod_values$maps[[input$map_type]][[input$period]]
    #   
    #   if (input$variable == "total_observation"){
    #     variable_to_plot = "total_observation"
    #   } else if (input$variable == "total_observation_"){
    #     variable_to_plot = paste0("total_observation_", input$espece_focale)
    #   } else {
    #     variable_to_plot = paste0("frequence_observation_", input$espece_focale)
    #   }
    #   
    #   variable_to_plot <- gsub(" ", "_", variable_to_plot)
    #   
    #   # valeur minimale à représenter
    #   map_to_plot <- map_to_plot |>
    #     dplyr::filter(total_observation > input$min_obs) |>
    #     dplyr::filter_at(dplyr::vars(variable_to_plot), dplyr::any_vars(. > 0))
    #   colnames(map_to_plot) <- gsub(" ", "_", colnames(map_to_plot))
    #   ggplot2::ggplot(map_to_plot, ggplot2::aes_string(x = variable_to_plot, fill = variable_to_plot)) +
    #     ggplot2::geom_histogram(fill = "#ebc034")
    #   
    # })
    
    
  })
}


