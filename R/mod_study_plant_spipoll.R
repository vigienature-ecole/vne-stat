mod_study_plant_spipoll_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
      selectizeInput(ns("taxon_select_plant"), "Sélectionner une plante", choices = "", multiple = FALSE, options = NULL),
      selectizeInput(ns("taxon_select_plant_2"), "Sélectionner une plante à comparer (facultatif)", choices = "", multiple = FALSE, options = NULL),
      selectInput(ns("correction"), "Sélectionner les métriques", c("Données brutes", "Données relatives"))
    ),
    column( width = 8,
            h3("Répartition spatiales des observations"),
            leaflet::leafletOutput(ns("map_collection")),
            h3("Répartition temporelles des observations"),
            
            h4("Par mois"),
            plotOutput(ns("coll_month")),
            h4('Par années'),
            plotOutput(ns("coll_year")),
            h3("répartition taxonomique des insectes observés"),
            h4("Par grands ordres"),
            plotOutput(ns("taxo_ordre")),
            h4("Par groupes spipoll"),
            plotOutput(ns("taxo_group"))
    )
  )
}

mod_study_plant_spipoll_server <- function(id, parent_session, data_values){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # values necessary for the module
    mod_values <- reactiveValues(data_values_filtered = NULL,
                                 data_values_filtered_2 = NULL,
                                 taxon_change = 1,
                                 data_collections = NULL,
                                 data_collections_2 = NULL)
    
    # update list plants for selection by the user
    observeEvent(mod_values$taxon_change, {
      updateSelectizeInput(session, "taxon_select_plant", choices = c("tout le programme",sort(data_values$spipoll$Plante)), server = TRUE)
      updateSelectizeInput(session, "taxon_select_plant_2",selected = "", choices = sort(data_values$spipoll$Plante), server = TRUE)
    })
    
    # filter spipoll data for the plant
    observeEvent(c(input$taxon_select_plant,input$taxon_select_plant_2), {
      cat("filter data\n")
      if (input$taxon_select_plant == "tout le programme"){
        mod_values$data_values_filtered <- data_values$spipoll
      } else {
        mod_values$data_values_filtered <- data_values$spipoll[Plante == input$taxon_select_plant, ]
      }
      mod_values$data_values_filtered_2 <- data_values$spipoll[Plante == input$taxon_select_plant_2, ]
      mod_values$data_collections <- dplyr::select(mod_values$data_values_filtered, Numero_observation, Latitude, Longitude, Mois, Annee, Plante)
      mod_values$data_collections_2 <- dplyr::select(mod_values$data_values_filtered_2, Numero_observation, Latitude, Longitude, Mois, Annee, Plante)
    })
    
    
    
    
    
    # carte leaflet de toutes les collections + collection qui contiennent la plante
    output$map_collection <- leaflet::renderLeaflet({
      if (!is.null(mod_values$data_collections)){
        if(nrow(mod_values$data_collections) ){
          cat("select only relevant dataset\n")
          to_map <- dplyr::select(mod_values$data_collections,Numero_observation, Longitude, Latitude) %>%
            dplyr::distinct()
          
          map <- leaflet::leaflet(to_map) %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, fillColor = "#62CC33", color = "#62CC33",  fillOpacity = 0.4, radius = 4)
          
          if(input$taxon_select_plant_2 != "") {
            to_map_2 <- dplyr::select(mod_values$data_collections_2,Numero_observation, Longitude, Latitude) %>%
              dplyr::distinct()
            map <- map %>%
              leaflet::addCircleMarkers(data = to_map_2, lng = ~Longitude, lat = ~Latitude, fillColor = "red", color = "red",  fillOpacity = 0.4, radius = 4)
          }
          cat("show map")
          map 
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    
    # plot des collections par mois
    output$coll_month <- renderPlot({
      #browser()
      if (!is.null(mod_values$data_collections)){
        if(nrow(mod_values$data_collections) ){
          
          total_program <- dplyr::select(mod_values$data_collections,Numero_observation, Mois) |>
            dplyr::distinct() |>
            dplyr::group_by(Mois) |>
            dplyr::summarise(Nombre_collection = dplyr::n())
          
          
          if (input$taxon_select_plant == "tout le programme") {
            to_plot <- total_program
            to_plot$Plante = "tout le programme"
          } else {
            to_plot <- dplyr::select(mod_values$data_collections,Numero_observation, Mois, Plante) |>
              dplyr::distinct() |>
              dplyr::group_by(Mois, Plante) |>
              dplyr::summarise(Nombre_collection = dplyr::n())
          }
          
          plot <- ggplot2::ggplot(to_plot, ggplot2::aes(x = Mois, y = Nombre_collection)) +
            ggplot2::expand_limits(y = 0) +
            ggplot2::labs(y = "Nombre de protocoles\nréalisés", x = "Mois de l'année") +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.text = ggplot2::element_text(size = 18),
                           legend.title =  ggplot2::element_text(size = 20),
                           axis.text=ggplot2::element_text(size = 20),
                           axis.title=ggplot2::element_text(size = 24),
                           strip.text.x = ggplot2::element_text(size = 20),
                           axis.title.x = ggplot2::element_text(vjust = -2),
                           axis.title.y = ggplot2::element_text(vjust = 2.3),
                           
                           plot.margin=ggplot2::unit(c(1,1,1.5,1.2),"cm"),
                           plot.caption = ggplot2::element_text(size = 16))
          
          if (input$taxon_select_plant == "tout le programme") {
            plot <- plot + ggplot2::geom_line() +
              ggplot2::geom_point()
          } else {
            plot <- plot + ggplot2::geom_line(aes(color = Plante)) +
              ggplot2::geom_point(aes(color = Plante))
          }
          
          if(input$taxon_select_plant_2 != "") {
            # browser()
            
            to_plot_2 <- dplyr::select(mod_values$data_collections_2,Numero_observation, Mois, Plante) |>
              dplyr::distinct() |>
              dplyr::group_by(Mois, Plante) |>
              dplyr::summarise(Nombre_collection = dplyr::n())
            
            plot <- plot +
              ggplot2::geom_line(data = to_plot_2) +
              ggplot2::geom_point(data= to_plot_2)
          }
          
          plot
          
          
        } else {
          NULL
        }
      } else {
        NULL
      }
    }) 
    
    # plot des collections par mois
    output$coll_year <- renderPlot({
      #browser()
      if (!is.null(mod_values$data_collections)){
        if(nrow(mod_values$data_collections) ){
          
          total_program <- dplyr::select(mod_values$data_collections,Numero_observation, Annee) |>
            dplyr::distinct() |>
            dplyr::group_by(Annee) |>
            dplyr::summarise(Nombre_collection = dplyr::n())
          
          
          if (input$taxon_select_plant == "tout le programme") {
            to_plot <- total_program
            to_plot$Plante = "tout le programme"
          } else {
            to_plot <- dplyr::select(mod_values$data_collections,Numero_observation, Annee, Plante) |>
              dplyr::distinct() |>
              dplyr::group_by(Annee, Plante) |>
              dplyr::summarise(Nombre_collection = dplyr::n())
          }
          
          plot <- ggplot2::ggplot(to_plot, ggplot2::aes(x = Annee, y = Nombre_collection)) +
            ggplot2::expand_limits(y = 0) +
            ggplot2::labs(y = "Nombre de protocoles\nréalisés", x = "Année") +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.text = ggplot2::element_text(size = 18),
                           legend.title =  ggplot2::element_text(size = 20),
                           axis.text=ggplot2::element_text(size = 20),
                           axis.title=ggplot2::element_text(size = 24),
                           strip.text.x = ggplot2::element_text(size = 20),
                           axis.title.x = ggplot2::element_text(vjust = -2),
                           axis.title.y = ggplot2::element_text(vjust = 2.3),
                           
                           plot.margin=ggplot2::unit(c(1,1,1.5,1.2),"cm"),
                           plot.caption = ggplot2::element_text(size = 16))
          
          if (input$taxon_select_plant == "tout le programme") {
            plot <- plot + ggplot2::geom_line() +
              ggplot2::geom_point()
          } else {
            plot <- plot + ggplot2::geom_line(aes(color = Plante)) +
              ggplot2::geom_point(aes(color = Plante))
          }
          
          if(input$taxon_select_plant_2 != "") {
            # browser()
            
            to_plot_2 <- dplyr::select(mod_values$data_collections_2,Numero_observation, Annee, Plante) |>
              dplyr::distinct() |>
              dplyr::group_by(Annee, Plante) |>
              dplyr::summarise(Nombre_collection = dplyr::n())
            
            plot <- plot +
              ggplot2::geom_line(data = to_plot_2) +
              ggplot2::geom_point(data= to_plot_2)
          }
          
          plot
          
          
        } else {
          NULL
        }
      } else {
        NULL
      }
    }) 
    # plot des interaction en fonction de l'ordre
    output$taxo_ordre <- renderPlot({
      #browser()
      if (!is.null(mod_values$data_collections)){
        if(nrow(mod_values$data_collections) ){
          
          to_plot <- mod_values$data_values_filtered |>
            dplyr::group_by(Ordre, Plante) |>
            dplyr::summarise(Nombre_interactions = dplyr::n())
          
          if (!input$taxon_select_plant == "tout le programme") {
            to_plot$Plante <- "tout le programme"
          }
          
          if(input$taxon_select_plant_2 != "") {
            # browser()
            
            to_plot_2 <- mod_values$data_values_filtered_2 |>
              dplyr::group_by(Ordre, Plante) |>
              dplyr::summarise(Nombre_interactions = dplyr::n())
            
            if (!input$taxon_select_plant == "tout le programme") {
              to_plot_2$Plante <- "tout le programme"
            }
            
            to_plot <- dplyr::bind_rows(to_plot, to_plot_2)
          }
          
          plot <- ggplot2::ggplot(to_plot, ggplot2::aes(y = reorder(Ordre, Nombre_interactions), x = Nombre_interactions, fill = Ordre)) +
            ggplot2::geom_col() +
            ggplot2::expand_limits(y = 0) +
            ggplot2::labs(x = "Nombre d'interactions\nobservées", y = "Ordre des \ninsectes observés") +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text=ggplot2::element_text(size = 20),
                           axis.title=ggplot2::element_text(size = 24),
                           strip.text.x = ggplot2::element_text(size = 20),
                           axis.title.x = ggplot2::element_text(vjust = -2),
                           axis.title.y = ggplot2::element_text(vjust = 2.3),
                           plot.margin=ggplot2::unit(c(1,1,1.5,1.2),"cm"),
                           legend.position = "none",
                           plot.caption = ggplot2::element_text(size = 16)) +
            ggplot2::facet_wrap(~Plante)
          
          plot
        }
      }
    })
    
    
    
    
    # plot  des interaction en fonction du group spipoll
    output$taxo_group <- renderPlot({
      #browser()
      if (!is.null(mod_values$data_collections)){
        if(nrow(mod_values$data_collections) ){
          
          to_plot <- mod_values$data_values_filtered |>
            dplyr::group_by(Espece, Ordre, Plante) |>
            dplyr::summarise(Nombre_interactions = dplyr::n())
          
          if(input$taxon_select_plant_2 != "") {
            # browser()
            
            to_plot_2 <- mod_values$data_values_filtered_2 |>
              dplyr::group_by(Espece, Ordre, Plante) |>
              dplyr::summarise(Nombre_interactions = dplyr::n())
            
            to_plot <- dplyr::bind_rows(to_plot, to_plot_2)
          }
          
          to_plot$Espece <- reorder(to_plot$Espece, to_plot$Nombre_interactions)
          
          # browser()
          ggplot2::ggplot(to_plot, ggplot2::aes(y = Espece, x = Nombre_interactions, fill = Espece)) +
            ggplot2::geom_col() +
            ggplot2::expand_limits(y = 0) +
            ggplot2::labs(x = "Nombre d'interactions\nobservées", y = "Groupes des insectes\nobservés") +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text=ggplot2::element_text(size = 20),
                           axis.title=ggplot2::element_text(size = 24),
                           strip.text.x = ggplot2::element_text(size = 20),
                           axis.title.x = ggplot2::element_text(vjust = -2),
                           axis.title.y = ggplot2::element_text(vjust = 2.3),
                           plot.margin=ggplot2::unit(c(1,1,1.5,1.2),"cm"),
                           legend.position = "none",
                           plot.caption = ggplot2::element_text(size = 16))
          
          
          
          
        }
      }
    }, height = 10000, width = 800)
    
  })
}