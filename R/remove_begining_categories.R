# remove 01_ from factors but keeps order
remove_begining_categories <- function (input, column_name){
  if(!class(input[[column_name]]) == "numeric"){
    
    input[[column_name]] <- factor(input[[column_name]])
    levels_column <- levels(input[[column_name]])
    levels(input[[column_name]]) <- stringr::str_replace_all(levels_column, ",", ",\n")
    
    if(any(grepl(pattern = "^[0-9][0-9]_", input[[column_name]]))){
      level_to_change = grep(pattern = "^[0-9][0-9]_", levels_column)
      levels_column[level_to_change] = substr(levels_column[level_to_change], 4, nchar(levels_column[level_to_change]))
      levels(input[[column_name]]) <- levels_column
    }
  }
  input
}
