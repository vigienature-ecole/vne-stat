library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr")                                    # Load readr package

# read and give stats on VNE
list_all_files <- paste0("data/",list.files("data", pattern = "5a9")) 
data_all <- list_all_files %>% 
  lapply(read_csv) %>%                                         # Combine data sets into one data set 
  bind_rows()

if ("user_data.csv" %in% list.files("data")){
  data_user <- read.csv("data/user_data.csv")
  data_all <- bind_rows(data_user, data_all)
}



write_csv(data_all, "data/user_data.csv")
#file.remove(list_all_files)
