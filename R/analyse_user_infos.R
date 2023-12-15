test = FALSE
if (test) {
  library(ggplot2)
  user_data <- data.table::fread("../../datasets/papers/user_data.csv")
  user_data_filtered <- user_data |>
    dplyr::filter(type_login != "Équipe VNE")
  
  total <- user_data_filtered |> 
    dplyr::group_by(type_login) |>
    dplyr::summarise(nombre = dplyr::n())
  
  ggplot(total, aes(y = reorder(type_login, nombre), x = nombre)) +
    geom_col() +
    ylab("Type d'utilisateur")
  
  type_precis <- user_data_filtered |>
    dplyr::filter(!is.na(type_precis_login)) 
  type_precis
  
  utilisation <- user_data_filtered |>
    dplyr::filter(!is.na(utilisation_detail)) 
  
  utilisation
  
}