####### Import all csv
#' @title import_mh_all_sample_csv
#' @description  import CPS and descriptive stats of all samples and modes from in the batch folder
#' @param path_batch the path to the target batch file.
#' @param modes_nr a numerical indicating the number of modes (e.g. NoGas, He, O2, H2 ect.) used in the batch



import_mh_all_sample_csv <- function(path_batch = "man/readme/extdata/random_ME_run.b/", modes_nr = 2){
  ### predefine variables as NULL
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
 (suppressMessages(
    suppressWarnings(
    y <- purrr::map(x,~import_mh_sample_csv(.x)) %>%
    dplyr::bind_rows()
    )))

  meta_data <- suppressWarnings(
    import_metadata(path_batch)
    )
  df <- y %>%
    dplyr::select(-"aq_time") %>%
    dplyr::left_join(meta_data,by = c("file_name"))

  names(df) <- stringr::str_replace_all(
    stringr::str_to_lower(names(df)),
    "\\.",
    "\\_"
  )

  return(df)
}
