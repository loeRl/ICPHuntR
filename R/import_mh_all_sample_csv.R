####### Import all csv
#' @title import_mh_all_sample_csv
#' @description  import CPS and descriptive stats of all samples and modes from in the batch folder
#' @param path_batch the path to the target batch file.
#' @param modes_nr a numerical indicating the number of modes (e.g. NoGas, He, ect.) used in the batch



import_mh_all_sample_csv <- function(path_batch = "man/readme/extdata/random_ME_run.b/", modes_nr = 2){
  aq.time = meta_data = . = NULL

  x <- c()
  i <- 1
  (list_samples <- list.files(path_batch) %>%
      subset(.,stringr::str_detect(., "\\.d")) %>%
      stringr::str_sub(end = -3L))

  while (i <= modes_nr) {
    x <- append(x,paste(path_batch,"/",list_samples,".d/",list_samples,"_",i,".csv",sep = ""))
    i <- i+1
  }
  y <- purrr::map(x,~import_mh_sample_csv(.x)) %>%
    dplyr::bind_rows()
  return(y)
  meta_data <- import_metadata(path_batch)
  df <- y %>%
    dplyr::select(-aq.time) %>%
    dplyr::left_join(meta_data,by = c("file_name"))
  return(df)
}
