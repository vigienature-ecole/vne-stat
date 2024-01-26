make_graph <- function(data_to_plot, variable_group, variable_info, current_dataset_name, index_type){
  # get variable information
  library(ggplot2)
  #mettre en place le titre de l'axe des x
  if(index_type == "abondance") {
    y_label = "Nombre moyen\nd'individus"
  } else if(index_type == "activity") {
    y_label = "Nombre moyen\nde contacts"
  } else if (index_type == "diversite") {
    y_label = "Nombre moyen\nd'espèces"
  } else if (index_type == "nombre_especes") {
    y_label = "Nombre\nd'observation où\nl'espèce est présente"
    variable_group = "Espece"
    variable_info <- data.frame(
      variable_order = "value", 
      label = "Espèce",
      variable_type  = "quali")
  } else {
    y_label = "Nombre\nd'observation"
  }
  
  # Filtrer les données manquantes ----
  
  if(variable_group != "Choisir une variable"){
    lenght_data <- nrow(data_to_plot)
    data_to_plot <- data_to_plot[data_to_plot[[variable_group]] !=  "07_non communiqué", ]
    data_to_plot <- data_to_plot[data_to_plot[[variable_group]] !=  "", ]
    if(lenght_data > nrow(data_to_plot)){
      info_filter <- "Des données ont été filtrées car elles contenaient des données non renseignées\n"
    }
    
    # Gestion des variables ----
    
    
    ## Gestion des mois ----
    if (variable_group == "Mois") {
      data_to_plot$Mois <- label_month(data_to_plot$Mois, short = TRUE, numbered_school_year = TRUE)
      # remove july and august
      if (current_dataset_name != "Spipoll"){
        data_to_plot <- data_to_plot[!Mois %in% c("12_Août", "11_Juil")]
        info_variable <- "Les données pour les mois de Juillet et d'Août ont été supprimées de ce graphique, car trop peu de protocoles sont réalisés à cette période pour obtenir des résultats significatifs.\n"
      }
    }
    
    #limitation du nombre de catégories
    
    if (variable_info$variable_type  == "quali" & nrow(data_to_plot) > 30) {
      data_to_plot <- data_to_plot[order(-index)][1:30]
    }
    
    x_label = variable_info$label
    if(any(grepl("_", data_to_plot[[variable_group]]))) {
      data_to_plot <- remove_begining_categories(data_to_plot, variable_group)
    }
    
    #mise en place du mapping
    if(variable_info$variable_order == "value"){
      graph <- ggplot(data_to_plot, aes(x = reorder(!!ensym(variable_group), index) , y = index, group = 1))
    } else {
      graph <- ggplot(data_to_plot, aes(x = !!ensym(variable_group), y = index, group = 1))
    }
    
  } else {
    graph <- ggplot(data_to_plot, aes(x = 1, y = index, group = 1))
    x_label = ""
  }
  
  
  ## Gestion des erreurs ----
  if ("erreur_marginale" %in% colnames(data_to_plot)) {
    cat("ajout des barres d'erreur")
    data_to_plot <- data_to_plot[ , error_plus := index + erreur_marginale]
    data_to_plot <- data_to_plot[ , error_moins := index - erreur_marginale]
    data_to_plot$error_moins[data_to_plot$error_moins < 0] <- 0
    
    error_bars <- geom_errorbar(data = data_to_plot, aes(ymax = error_plus, ymin = error_moins),
                                linewidth = 1.2)
  }
  
  ## Choix du type de graphique ----
  
  if(variable_group != "Choisir une variable"){
    if (variable_info$variable_type == "quali"){
      graph <- graph + 
        geom_col(aes(fill = !!ensym(variable_group)))
      if ("erreur_marginale" %in% colnames(data_to_plot)) {
        graph = graph + error_bars
      }
    } else if (variable_info$variable_type == "quanti_points"){
      if(index_type == "activity") {
        library(scales)
        expm1_trans <-  function() scales::trans_new("expm1", "expm1", "log1p")
    
        graph <- graph + 
          geom_smooth() +
          scale_y_continuous(trans=scales::log1p_trans()) +
          coord_trans(y=expm1_trans())
          # scale_y_log10() +
          # coord_trans(ytrans="pow10")
      } else {
        graph <- graph + 
          geom_point() +
          geom_smooth()
      }
      
      if ("erreur_marginale" %in% colnames(data_to_plot)) {
        graph = graph + error_bars
      }
    } else if (variable_info$variable_type == "quanti_line"){
      graph <- graph + 
        geom_line(linewidth = 2, col = "red")
      if ("erreur_marginale" %in% colnames(data_to_plot)) {
        graph = graph + error_bars
      }
      graph <- graph + 
        geom_point(size = 3, col = "red")
    }
  } else {
    graph <- graph + 
      geom_col(fill = "#62CC33")
  }
  
  #reduire la taille des étiquettes
  # reduce size if a lot of labels
  if (nrow(data_to_plot) > 20) {
    label_size = 16
  } else {
    label_size = 20
  }
  
  #Ajouter un theme
  # add theme
  graph <- graph +
    expand_limits(y = 0) +
    labs(y = y_label, x = x_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text=element_text(size = label_size),
                   axis.title=element_text(size = 24),
                   strip.text.x = element_text(size = 20),
                   axis.title.x = element_text(vjust = -2),
                   axis.title.y = element_text(vjust = 2.3),
                   plot.margin=unit(c(1,1,1.5,1.2),"cm"),
                   legend.position = "none",
                   plot.caption = element_text(size = 16))
  
  if(variable_group == "Choisir une variable"){
    graph <- graph + ggplot2::theme(strip.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text.x = element_blank())
    
  } else {
    # rotate long labels
    if (nrow(data_to_plot) > 5 & variable_info$variable_type == "quali"){
      graph <- graph + theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))
    }
  }
  
  # add disclaimer
  
  graph <- graph + ggplot2::labs(caption = paste("\nCe graphique est issu d'une étude exploratoire et ne peut être en aucun cas utilisé pour directement publier des résultats", collapse = "\n"))
  
  
  return(graph)
}