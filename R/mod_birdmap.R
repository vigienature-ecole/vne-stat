mod_map_birds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
      h3("Paramètres de la carte"),
      selectInput(ns("map_type"), "Choix du type de carte", choices = setNames(c("carte_grille_100km",
                                                                                 "carte_grille_50km",
                                                                                 "carte_grille_25km",
                                                                                 "carte_grille_10km",
                                                                                 "carte_academie",
                                                                                 "carte_departement",
                                                                                 "carte_region",
                                                                                 "carte_france"),
                                                                               c("Grille de 100km x 100km",
                                                                                 "Grille de 50km x 50km",
                                                                                 "Grille de 25km x 25km",
                                                                                 "Grille de 10km x 10km",
                                                                                 "Carte par academie",
                                                                                 "Carte par departement",
                                                                                 "Carte par region",
                                                                                 "Carte de la france"))),
      selectInput(ns("variable"), "Choix de la variable à représenter", choices = setNames(c(
        "frequence_observation",
        "total_observation"
      ), c(
        "Frequence d'observation de l'espèce",
        "Total des observations"
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
      
      
      selectInput(ns("espece_focale"), "Choix de l'espèce à représenter", choices = sort(c("Accenteur mouchet", 
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
            htmlOutput(ns("title"))%>% 
              tagAppendAttributes(style = 'color:#ff6666;font-weight: bolder;font-size: x-large;'),
            uiOutput(ns("bird_image")),
            
            # ),
            plotOutput(ns("map"))
            
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
    
    observeEvent(input$variable,{
      if (input$variable == "total_observation"){
        hide("espece_focale")
      } else {
        show("espece_focale")
      }
    })
    
    output$map <- renderPlot({
      # select map
      map_to_plot <- mod_values$maps[[input$map_type]][[input$period]]
      
      if (input$variable == "total_observation"){
        variable_to_plot = "total_observation"
      } else {
        variable_to_plot = paste0("frequence_observation_", input$espece_focale)
      }
      
      
      # valeur minimale à représenter
      map_to_plot <- map_to_plot |>
        dplyr::filter(total_observation > input$min_obs)
      
      # change month from num to letters
      if(input$period == "mois"){
        map_to_plot$mois <- label_mounth(map_to_plot$mois)
      }
      
      # define theme for map (legend, remove axis, colors)
      theme_map <- function(...) {
        theme_minimal() +
          theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 20, face = "bold",
                                      hjust=0.5,family="Arial Narrow",color="gray35"),
            plot.subtitle = element_text(size = 14,hjust=0.5,
                                         family="Arial Narrow",color="gray35"),
            plot.caption = element_text(size=10,
                                        family="Arial Narrow",color="gray35"),
            strip.text.x = element_text(size=14,hjust=0.1,vjust=0, face = "bold",
                                        family="Arial Narrow",color="gray35"),
            plot.margin = margin(0.8, 0.5, 0.5, 0.5, "cm"),
            panel.border = element_blank(),
            legend.position = 'bottom',
            legend.title = element_text(family="Arial Narrow",color="gray35"),
            legend.text = element_text(family="Arial Narrow",color="gray35")
          )}
      
      
      map_ggplot <- ggplot2::ggplot(map_to_plot) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable_to_plot]]), color = NA) +
        ggplot2::geom_sf(data = mod_values$carte_france, fill = NA, lwd = 0.7)
      
      if(input$period != "all"){
        map_ggplot <- map_ggplot + ggplot2::facet_wrap(as.formula(paste0("~", input$period)), nrow = 2 )
      }
      
      if (input$variable == "total_observation"){
        variable_title = "Total des observations"
      } else {
        variable_title = paste("Fréquence d'observation de l'espèce : ", input$espece_focale)
      }
      
      map_ggplot <- map_ggplot + 
        viridis::scale_fill_viridis(alpha=0.80,na.value='#f5f5f2') +
        theme_map() +
        ggplot2::guides(fill = ggplot2::guide_colourbar(direction = 'horizontal',  ## transform legend
                                      title=variable_title,  ##rename default legend
                                      title.position='top',
                                      title.hjust=0.5,
                                      ticks.colour='#f5f5f2',
                                      ticks.linewidth=2,
                                      barwidth = 20,
                                      barheight = 1))
      
      map_ggplot
      
      
      # # carte à réaliser
      # observation_map <- tmap::tm_shape(mod_values$carte_france) +  
      #   tmap::tm_borders()
      # 
      # 
      # observation_map <-  observation_map + 
      #   tmap::tm_shape(map_to_plot) +  
      #   tmap::tm_fill(col = variable_to_plot, n = 10) 
      # 
      # if(input$period != "all"){
      #   observation_map <- observation_map + tmap::tm_facets(by = input$period, free.coords = FALSE)
      # } 
      # 
      # observation_map <- observation_map +
      #   tmap::tm_shape(mod_values$carte_france) +  
      #   tmap::tm_borders() +
      #   tmap::tm_layout(frame = FALSE, legend.outside = TRUE, frame.lwd = NA, panel.label.bg.color = NA, panel.label.size = 2)
      # 
      # observation_map
    })
    
    output$title <- renderText({
      if (input$variable == "total_observation"){
        "Total des observations"
      } else {
        paste("Fréquence d'observation de l'espèce : ", input$espece_focale)
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


