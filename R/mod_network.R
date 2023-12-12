# This module allow the user to login
# on validation the rest of the app is loaded

library(magrittr)
library(shinydashboard)

mod_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      actionButton(
        ns("view_network"), 
        "Afficher le réseaux / Appliquer les paramètres", 
        style="color: #fff; background-color: #62CC33; border-color: #62CC3300; font-size:120%"
      ),
      style='min-height:500px; border: 10px solid white; padding: 10px; border-radius: 20px; background: #DDEDDD', width = 4, align="center",
      h3("Paramètres de la représentation du réseau"),
      selectInput(ns("taxon_depth_insect"), "Niveau taxonomique des insectes et araignées", choices = setNames(c("Ordre", "Espece"), c("Grands groupes d'insectes et araignées", "Espèces d'insectes et araignées"))),
      selectInput(ns("taxon_depth_plant"), "Niveau taxonomique des plantes", choices = setNames(c("Famille_plante", "Plante"), c("Grands groupes de plantes (Familles)", "Espèces de plantes"))),
      selectizeInput(ns("taxon_select_insect"), "Sélectionner des insectes et araignées", choices = "", multiple = TRUE, options = NULL),
      helpText("Si laissé vide, alors tous les insectes et/ou araignées sont affichés (selon le nombre d'intéraction minimum), cela permet de faire des comparaisons précises ou de regarder des réseaux plus simples"),
      selectizeInput(ns("taxon_select_plant"), "Sélectionner des plantes", choices = "", multiple = TRUE, options = NULL),
      helpText("Si laissé vide, alors toutes les plantes sont affichées (selon le nombre d'intéraction minimum), cela permet de faire des comparaisons précises ou de regarder des réseaux plus simples"),
      sliderInput(ns("max_interactions"),
                  "Nombre d'interactions représentées",
                  min = 2,
                  max = 3000,
                  value = 50),
      helpText("Ce champ permet de choisir le nombre d'intéractions représentées. Cela permet de réduire la complexité du réseau mais biaise les données pour les organismes les plus abondants. Vous pouvez utiliser les filtres par espèces pour regarder les organismes plus rares."),
      checkboxInput(ns("normalise_interactions_plant"), label = "Normaliser les intéractions selon les plantes", value = TRUE),
      helpText("Si l'on utilise cette option, le nombre d'intéractions est divisé par le nombre total d'intéractions observées sur cette plante. Attention certaines plantes (comme le Lierre grimpant ou la carotte sauvage sont très représentés tandis que d'autres n'ont été vues qu'une seule fois) ce qui peut biaiser les résultats.")
      
    ),
    column( width = 8,
            htmlOutput(ns("plant_title")) %>% 
              tagAppendAttributes(style = 'color: green; font-weight: bolder; text-align: center; font-size:200%;'),
            plotOutput(ns("interaction_plot")),
            div(style = "height:300px"),
            htmlOutput(ns("insect_title")) %>% 
              tagAppendAttributes(style = 'color: green; font-weight: bolder; text-align: center; font-size:200%'),
            uiOutput(ns("boxes_insects")),
            uiOutput(ns("boxes_plants"))
            
    )
  )
}


mod_network_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Variables for module
    cat("start init data and values for insect network")
    mod_values <- reactiveValues(
      taxon_change = 1,
      filter_change = 1,
      interactions = readRDS("../../datasets/papers/spipoll_interaction.rds"),
      insect_images = readRDS("../../datasets/papers/spipoll_insect_images.rds"),
      plant_images = readRDS("../../datasets/papers/spipoll_plant_images.rds"),
      current_full_network = NULL,
      current_filtered_taxon_network = NULL,
      current_filtered_taxon_network_max = NULL,
      interaction_matrix = NULL
    )
    cat("    ok\n")
    
    observeEvent(input$taxon_depth_insect, {
      cat("change insect taxo\n")
      mod_values$taxon_change = mod_values$taxon_change + 1
    })
    
    observeEvent(input$taxon_depth_plant, {
      cat("change plant taxo\n")
      mod_values$taxon_change = mod_values$taxon_change + 1
    })
    
    observeEvent(mod_values$taxon_change, {
      cat("change in taxo detected\n")
      # choose taxonomic level
      current_full_network <- mod_values$interactions[[paste0(input$taxon_depth_insect, "_", input$taxon_depth_plant)]]
      
      mod_values$current_full_network <- current_full_network
      
      updateSelectizeInput(session, "taxon_select_plant", choices = sort(dplyr::pull(current_full_network[, 2])), server = TRUE)
      updateSelectizeInput(session, "taxon_select_insect", choices = sort(dplyr::pull(current_full_network[, 1])), server = TRUE)
      mod_values$filter_change = mod_values$filter_change + 1
    })
    
    observeEvent(input$taxon_select_insect, {
      cat("change insect filter\n")
      mod_values$filter_change = mod_values$filter_change + 1
    })
    
    observeEvent(input$taxon_select_plant, {
      cat("change plant filter\n")
      mod_values$filter_change = mod_values$filter_change + 1
    })
    
    observeEvent(mod_values$filter_change, {
      cat("change filter detected \n")
      current_filtered_taxon_network <- mod_values$current_full_network
      
      if(!is.null(input$taxon_select_plant)){
        if (length(input$taxon_select_plant) > 0) {
          cat("effective filtering plant\n")
          current_filtered_taxon_network <- subset(current_filtered_taxon_network, dplyr::pull(current_filtered_taxon_network[, input$taxon_depth_plant]) %in% input$taxon_select_plant)
        }
      }
      
      if(!is.null(input$taxon_select_insect)){
        if (length(input$taxon_select_insect) > 0) {
          cat("effective filtering insect\n")
          current_filtered_taxon_network <- subset(current_filtered_taxon_network, dplyr::pull(current_filtered_taxon_network[, input$taxon_depth_insect]) %in% input$taxon_select_insect)
        }
      }
      updateSliderInput(session, "max_interactions",
                        min = 2,
                        max = ifelse(nrow(current_filtered_taxon_network) > 300, 300, nrow(current_filtered_taxon_network)),
                        value = ifelse(nrow(current_filtered_taxon_network) < 50, nrow(current_filtered_taxon_network), 50))
      
      
      mod_values$current_filtered_taxon_network <- current_filtered_taxon_network
    })
    
    filtered_network_max_interactions <- reactive({
      current_filtered_taxon_network <- mod_values$current_filtered_taxon_network
      
      current_filtered_taxon_network <-  head(mod_values$current_filtered_taxon_network, input$max_interactions)
      current_filtered_taxon_network
    })
    
    # select taxonomic level of the network
    observeEvent(input$view_network, {
      
      cat("render results")
      
      
      current_filtered_taxon_network <- filtered_network_max_interactions()
      
      
      
      if (input$normalise_interactions_plant){
        
        # calculate total interaction (for normalisation)
        plant_total_interactions <- current_filtered_taxon_network %>%
          dplyr::group_by_at(input$taxon_depth_plant) %>%
          dplyr::summarise(Nombre_total = sum(nombre)) %>%
          dplyr::arrange(desc(Nombre_total))
        
        # join for normalisation
        interaction_limited <- dplyr::inner_join(current_filtered_taxon_network, plant_total_interactions, by = input$taxon_depth_plant) %>%
          dplyr::mutate(interaction_value = nombre / Nombre_total) %>%
          dplyr::select(-nombre, -Nombre_total)
      } else {
        interaction_limited <- dplyr::rename(current_filtered_taxon_network, interaction_value = nombre)
      }
      
      
      
      interaction_limited <- subset(interaction_limited, interaction_limited[, input$taxon_depth_plant] != "")
      
      interaction_matrix <- interaction_limited %>%
        tidyr::pivot_wider(names_from = 2, values_from = interaction_value, values_fill = 0)
      
      interaction_matrix <- data.frame(interaction_matrix)
      row.names(interaction_matrix) <- interaction_matrix[, input$taxon_depth_insect]
      interaction_matrix <- interaction_matrix[, -1]
      
      mod_values$interaction_matrix <- as.matrix(interaction_matrix)
      
      
    })
    
    output$interaction_plot <- renderPlot({
      if(!is.null(mod_values$interaction_matrix)) {
        interaction_matrix <- mod_values$interaction_matrix 
        
        
        
        # Load RColorBrewer
        library(RColorBrewer)
        
        # Classic palette BuPu, with 4 colors
        col_plant <- brewer.pal(4, "PuOr") 
        col_plant <- c('#8E0152','#C51B7D','#DE77AE','#F1B6DA','#FDE0EF','#F7F7F7','#E6F5D0','#B8E186','#7FBC41','#4D9221','#276419')
        
        # Add more colors to this palette :
        col_plant <- colorRampPalette(col_plant)(ncol(interaction_matrix))
        
        # Classic palette BuPu, with 4 colors
        col_insect <- brewer.pal(4, "BrBG") 
        col_insect <- c('#7F3B08','#B35806','#E08214','#FDB863','#FEE0B6','#F7F7F7','#D8DAEB','#B2ABD2','#8073AC','#542788','#2D004B')
        
        # Add more colors to this palette :
        col_insect <- colorRampPalette(col_insect)(nrow(interaction_matrix))
        
        # bipartite_D3(interaction_matrix, 
        #              EdgeMode = 'straight', 
        #              colouroption = 'brewer',
        #              MainFigSize = c(800, 1500), 
        #              IndivFigSize = c(200, 600),
        #              BoxLabPos = c(20, 20),
        #              PercPos = c(200,200),
        #              BarSize = 20,
        #              MinWidth = 5,
        #              Pad=5)
        bipartite::plotweb(interaction_matrix,
                           text.rot = 90,
                           adj.high = 0,
                           adj.low = 1,
                           labsize = 1.7,
                           col.high = col_plant,
                           col.low = col_insect,
                           y.lim=c(-3,3))
      }
    }, height = 900)
    
    
    observeEvent(input$view_network, {
      output$boxes_insects <- renderUI({
        if(!is.null(mod_values$current_filtered_taxon_network)){
          interaction_limited <- filtered_network_max_interactions()
          insect_images <- mod_values$insect_images
          taxo_level <- input$taxon_depth_insect
          
          
          selected_insects <- unique(interaction_limited[[taxo_level]])
          image_list <- insect_images[[taxo_level]]
          selected_insect_images <- image_list[image_list[[taxo_level]] %in% selected_insects, ]
          selected_insect_images <- selected_insect_images[order(selected_insect_images[[taxo_level]])]
          
          lapply(seq_along(1:nrow(selected_insect_images)), function(a) {
            box(title = selected_insect_images[[taxo_level]][a], 
                width = 4, 
                status = "warning", 
                solidHeader = TRUE, 
                tags$img(src = selected_insect_images$image_url[a]
                         , width = "100%"
                )
            )
          })
        }
      })
      
      output$boxes_plants <- renderUI({
        if(!is.null(mod_values$current_filtered_taxon_network)){
          interaction_limited <- filtered_network_max_interactions()
          
          plant_images <- mod_values$plant_images
          taxo_level <- input$taxon_depth_plant
          
          
          selected_plants <- unique(interaction_limited[[taxo_level]])
          image_list <- plant_images[[taxo_level]]
          selected_plant_images <- image_list[image_list[[taxo_level]] %in% selected_plants, ]
          selected_plant_images <- selected_plant_images[order(selected_plant_images[[taxo_level]])]
          
          lapply(seq_along(1:nrow(selected_plant_images)), function(a) {
            box(title = selected_plant_images[[taxo_level]][a], 
                width = 4, 
                status = "success", 
                solidHeader = TRUE, 
                tags$img(src = selected_plant_images$image_url[a]
                         , width = "100%"
                )
            )
          })
        }
      })
      
      output$plant_title <- renderText({
        if (input$taxon_depth_plant == "Famille_plante"){
          "Grand groupes (Familles) de plante"
        } else {
          "Espèces de plante"
        }
      })
      
      output$insect_title <- renderText({      
        if (input$taxon_depth_insect == "Ordre"){
          "Ordre d'insectes et araignées"
        } else {
          "Espèces et groupe d'insectes et araignées"
        }
      })
      
    })
  })
}
