# prepare data


  


# importation des données
spipoll_data <- data.table::fread("../../datasets/papers/spipoll.csv")

taxon_level_insect <- c("Ordre", 
                  # "Genre", 
                  # "Famille", 
                  # "super_famille", 
                  # "infra_ordre", 
                  # "sous_ordre", 
                  # "Classe", 
                  "Espece")

insect_images <- list()
for (i in seq_along(taxon_level_insect)){
  taxon_level <- taxon_level_insect[i]
  insect_images[[i]] <- spipoll_data[, .(image_url = sample(Photo_taxon, size = 1)), by = taxon_level]
}
names(insect_images) <- taxon_level_insect


taxon_level_plant <- c(# "Ordre", 
                             # "Genre", 
                             "Famille_plante", 
                             # "super_famille", 
                             # "infra_ordre", 
                             # "sous_ordre", 
                             # "Classe", 
                             "Plante")

plant_images <- list()
for (i in seq_along(taxon_level_plant)){
  taxon_level <- taxon_level_plant[i]
  plant_images[[i]] <- spipoll_data[, .(image_url = sample(Photo_fleur, size = 1)), by = taxon_level]
}
names(plant_images) <- taxon_level_plant


get_interaction_data <- function(taxon_level_insect, taxon_level_plant) {
  interaction_data_all <- spipoll_data %>%
    dplyr::group_by_at(dplyr::all_of(c(taxon_level_insect, taxon_level_plant))) %>%
    dplyr::summarise(nombre = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(nombre))
  interaction_data <- na.omit(interaction_data_all)
}

interactions <- list()
for (i in seq_along(taxon_level_plant)) {
  for (j in seq_along(taxon_level_insect)) {
    interactions[[(i-1)*i + j]] <- get_interaction_data(taxon_level_insect[j], taxon_level_plant[i])
  }
}

names(interactions) <- paste0(rep(taxon_level_insect, 2), "_", rep(taxon_level_plant, each = 2))

# enregistrer les 3 listes
saveRDS(interactions, "../../datasets/papers/spipoll_interaction.rds")
saveRDS(insect_images, "../../datasets/papers/spipoll_insect_images.rds")
saveRDS(plant_images, "../../datasets/papers/spipoll_plant_images.rds")



