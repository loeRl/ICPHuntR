#'@title import_mh_sample_csv
#'@description import CPS and descriptive stats of a single sample from in the batch folder
#'@param csv_path path to the csv file of the sample
#'@export
#'
#'


import_mh_sample_csv <- function(csv_path = "man/readme/extdata/random_001SMPL_1.csv"){
  Mass = 'Time(Sec)' = X4 = aq.time = file.name = NULL

  ### extract the mode
  mode <- readr::read_delim(csv_path,
                            "/"
                            ,
                            col_names = FALSE,
                            trim_ws = TRUE,
               show_col_types = FALSE)

  ### extract the time_batch
  time_batch <- readr::read_delim(csv_path,
                                  ",",
                                  col_names = FALSE,
                                  trim_ws = TRUE,
                                  skip =1,
                                  show_col_types = FALSE)

  #### put everything together
  (df <- readr::read_csv(csv_path,
                         skip = 7,
                         show_col_types = FALSE
  ) %>%
      dplyr::filter(stringr::str_detect(Mass, "Printed", T)) %>%
      dplyr::mutate(
        aq.mode = as.character(mode[1,2]),
        aq.time = as.POSIXct(as.character(time_batch[1,2])),
        aq.batch = as.character(time_batch[1,4]),
        file_name =  stringr::str_extract(mode[1,1],"(?<=:)[:graph:]+")
      ) %>%
      dplyr::rename(
        det.mode =...4,
        RSD = "RSD[%]",
        int.time = `Time(Sec)`
      ))
  names(df) <- stringr::str_replace_all(
      stringr::str_to_lower(names(df)),
      "\\.",
      "\\_"
  )
  return(df)
}


