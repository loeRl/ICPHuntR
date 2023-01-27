#'
#'@title which_col_names_pos
#'@description this function returns a vector containing column name positions (This function is normally not exported)
#'@param .path path to a masshunter exportsheet in .xlsx or .xls format
#'@details this is an essential function for create_new_function_export
#'@export
#'

which_col_names_pos <- function(.path = "man/readme/extdata/random_ICP_8900_run.xlsx"){
  .df <- readxl::read_xlsx(.path) #import sheet
  names <- names(.df) ### extract col names
  a <- stringr::str_which(names, "\\.\\.\\.",negate = T) ### find empty cells
  b <- 1:length(names) ### find empty cells

  nexList <- list() ### itterate
  for (i in 1:length(b)){
    for(j in 1:length(a)){
      if (j < length(a)){
        if((b[i] >= a[j]) & (b[i] < a[j+1])){
          nexList <- c(nexList, a[j])
        }
      }
      else{
        if(b[i] >= a[(j)]){
          nexList <- c(nexList,a[length(a)])
        }
      }
    }
  }

  return(unlist(nexList))
}


