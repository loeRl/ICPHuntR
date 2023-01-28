#' A function rather aimed at developers
#' @title Assigning new names
#' @description A function that imports a Masshunter Export sheet and assigns new colnames.
#' This mainly resolves the issue of joint cells in the Masshunter export sheet. (This function can also be exported is not exported)
#' @param .df path to a masshunter exportsheet in .xlsx or .xls format
#' @keywords internal
#' @details
#' @export




assign_new_names <- function(.df= "man/readme/extdata/random_ICP_8900_run.xlsx"){
  a <- readxl::read_xlsx(.df) ### import an Agilent MassHunter Exportsheet
  .col_names_pos <- ICPHuntR::which_col_names_pos(.df) ### define colums to select
  names(a) <- stringr::str_remove(paste(names(a[.col_names_pos]),a[1,]),"Sample") #### extract and reasign names
  b <- a[-1,] ### delet the unwanted first header-
  return(b)
}
