calculate_indices <- function(data, index, variable_group){
  # choose grouping values
  if(variable_group == "Choisir une variable") {
    group_variable <- "Numero_observation"
  } else {
    group_variable <- c("Numero_observation", variable_group)
  }
  
  # if subobservation level
  
  if (!"Nombre_individus" %in% colnames(data)){
    if ("Nombre_contacts" %in% colnames(data)){
      data$Nombre_individus <- data$Nombre_contacts
    } else{
      data$Nombre_individus <- 1
    }
  }
  
  if (index == "abondance" | index == "activity"){
    # calcul abondance par observation
    biodiversity_index = data[ , .(index = sum(Nombre_individus, na.rm = TRUE)),
                               by = group_variable]
    if("Placette" %in% colnames(data)) {
      biodiversity_index$index <- biodiversity_index$index/3
    }
    
  } else {
    # attention probalement pas lichen go proof !!!!
    if("Numero_quadrat" %in% colnames(data) | "Placette" %in% colnames(data)){
      if("Numero_quadrat" %in% colnames(data)){
        group <- c(group_variable, "Numero_quadrat")
      } else {
        group <- c(group_variable, "Placette")
      }
      biodiversity_index_subplot = data[ , .(index = length(Nombre_individus[Nombre_individus > 0])),
                                         by = group]
      biodiversity_index = biodiversity_index_subplot[ , .(index = mean(index)),
                                                       by = group_variable]
    } else {
      
      biodiversity_index = data[ , .(index = length(Nombre_individus[Nombre_individus > 0])),
                                 by = group_variable]
    }
  }
  
  
  group_variable <- group_variable[-1]
  
  no_group = FALSE
  if (length(group_variable) > 0){
    if (group_variable %in% c("Latitude", "Longitude", 
                              "Longueur_rue", 
                              "Longitude_debut", "Latitude_debut", "Temperature_debut", 
                              "Eclairage_M", "Eclairage_L",
                              "Temperature_fin", "Proportion_artificiel", "Proportion_agricole", 
                              "Proportion_foret_semi_naturel", "Proportion_eau", "Altitude", "Proportion_zone_humide")) {
      no_group = TRUE
    }
  }
  
  if (!no_group){
    result_manip <- biodiversity_index[ , .(somme_totale = sum(index, na.rm = TRUE),
                                            nombre_participation = length(index),
                                            index = mean(index, na.rm = TRUE),
                                            ecart_type = sd(index,na.rm = TRUE),
                                            erreur_marginale = margin_error(index)
    ), 
    by = group_variable]
    result_manip_view = result_manip
    result_manip_view$intervalle_confiance = paste0("(",
                                                    round(result_manip$index - result_manip$erreur_marginale, 2),
                                                    " - ",
                                                    round(result_manip$index + result_manip$erreur_marginale,2),
                                                    ")")
    
    # remove erreur
    result_manip_view = result_manip_view[, erreur_marginale:=NULL]
    
    
    columns_to_round <- c("index", "ecart_type")
    result_manip_view <- result_manip_view[, (columns_to_round) := lapply(.SD, function (x) {round(x, 2)}), .SDcols = columns_to_round]
    
    if (index == "abondance"){
      colnames(result_manip_view) <- c(group_variable,
                                       "Somme des individus",
                                       "Nombre de participations", 
                                       "Nombre moyen d'individus", 
                                       "Écart-type du nombre d'individus",
                                       "Intervalle de confiance")
    } else if (index == "activity"){
      colnames(result_manip_view) <- c(group_variable,
                                       "Somme des contacts",
                                       "Nombre de participations", 
                                       "Nombre moyen de contacts", 
                                       "Écart-type du nombre de contacts",
                                       "Intervalle de confiance")
    } else if (index == "diversite"){
      colnames(result_manip_view) <- c(group_variable,
                                       "Somme du nombre d'espèces",
                                       "Nombre de participations", 
                                       "Nombre moyen d'espèces", 
                                       "Écart-type du nombre d'espèces",
                                       "Intervalle de confiance")
    } else {
      result_manip <- result_manip[ , `:=`(somme_totale = NULL, index = NULL, ecart_type = NULL, erreur_marginale = NULL)]
      colnames(result_manip)[colnames(result_manip) == "nombre_participation"] <- "index"
      result_manip_view <- result_manip_view[ , `:=`(somme_totale = NULL, index = NULL, ecart_type = NULL, intervalle_confiance = NULL)]
      colnames(result_manip_view) <- c(group_variable,
                                       "Nombre de sessions d'observation")
    }
  } else {

    result_manip <- biodiversity_index
    result_manip_view = result_manip
    if (index == "abondance"){
      colnames(result_manip_view) <- c("Numero_observation",
                                       group_variable,
                                       "Nombre d'individus")
    } else if (index == "activity"){
      colnames(result_manip_view) <- c("Numero_observation",
                                       group_variable,
                                       "Nombre de contact")
    } else if (index == "diversite"){
      colnames(result_manip_view) <- c("Numero_observation",
                                       group_variable,
                                       "Nombre d'espèces")
    } else {
      colnames(result_manip_view) <- c("Numero_observation",
                                       group_variable,
                                       "Nombre de sessions d'observation")
    }
  }
  
  results <- list(result_manip, result_manip_view)
  names(results) <- c("result_manip", "view")
  
  return(results)
}