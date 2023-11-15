# label each mounth with the name in French
label_mounth <- function(x){
  label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                         "1" = "01_Janvier", 
                         "2" = "02_Février", 
                         "3" = "03_Mars", 
                         "4" = "04_Avril", 
                         "5" = "05_Mai", 
                         "6" = "06_Juin", 
                         "7" = "07_Juillet", 
                         "8" = "08_Août", 
                         "9" = "09_Septembre", 
                         "10" = "10_Octobre",
                         "11" = "11_Novembre", 
                         "12" = "12_Décembre"
  )
  label
}