#'
#'@title which_col_names_pos
#'@description this function returns a vector containing column name positions (This function is not exported)
#'@param .path path to a masshunter exportsheet in .xlsx or .xls format
#'@details this is an essential function for create_new_function_export
#'
#'

which_col_names_pos <- function(.path){
  .df <- readxl::read_xlsx(.path)
  names <- names(.df)
  a <- stringr::str_which(names, "\\.\\.\\.",negate = T)
  b <- 1:length(names)

  nexList <- list()
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


