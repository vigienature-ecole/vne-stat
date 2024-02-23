run = FALSE

if (run){
  
  # read and give stats on VNE
  files <- list.files("data/data_users_temp/", pattern = "5a9")
  if(length(files ) > 0){
    list_all_files <- paste0("data/data_users_temp/", files) 
    data_all <- list_all_files |>
      lapply(function(x) readr::read_csv(x, col_types="ccct")) |>                                         # Combine data sets into one data set 
      dplyr::bind_rows()
    
    if ("user_data.csv" %in% list.files("data")){
      data_user <- read.csv("data/user_data.csv")
      data_all <- dplyr::bind_rows(data_user, data_all)
    }
    
    
    
    readr::write_csv(data_all, "data/user_data.csv")
    file.remove(list_all_files)
  }
}

