# label each mounth with the name in French
label_month <- function(x, short = FALSE, numbered_school_year = FALSE){
  if (short) {
    if (numbered_school_year){
      label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                             "1" = "05_Jan", 
                             "2" = "06_Fév", 
                             "3" = "07_Mar", 
                             "4" = "08_Avr", 
                             "5" = "09_Mai", 
                             "6" = "10_Juin", 
                             "7" = "11_Juil", 
                             "8" = "12_Août", 
                             "9" = "01_Sept", 
                             "10" = "02_Oct",
                             "11" = "03_Nov", 
                             "12" = "04_Déc"
      )
    } else {
      label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                             "1" = "Jan", 
                             "2" = "Fév", 
                             "3" = "Mar", 
                             "4" = "Avr", 
                             "5" = "Mai", 
                             "6" = "Juin", 
                             "7" = "Juil", 
                             "8" = "Août", 
                             "9" = "Sept", 
                             "10" = "Oct",
                             "11" = "Nov", 
                             "12" = "Déc"
      )
    }
  } else {
    if (numbered_school_year){
      label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                             "1" = "05_Janvier", 
                             "2" = "06_Février", 
                             "3" = "07_Mars", 
                             "4" = "08_Avril", 
                             "5" = "09_Mai", 
                             "6" = "10_Juin", 
                             "7" = "11_Juillet", 
                             "8" = "12_Août", 
                             "9" = "01_Septembre", 
                             "10" = "02_Octobre",
                             "11" = "03_Novembre", 
                             "12" = "04_Décembre"
      )
    } else {
      label = hutils::Switch(as.character(x), DEFAULT = "Non Déterminé",
                             "1" = "Janvier", 
                             "2" = "Février", 
                             "3" = "Mars", 
                             "4" = "Avril", 
                             "5" = "Mai", 
                             "6" = "Juin", 
                             "7" = "Juillet", 
                             "8" = "Août", 
                             "9" = "Septembre", 
                             "10" = "Octobre",
                             "11" = "Novembre", 
                             "12" = "Décembre"
      )
    }
  }
  label
}