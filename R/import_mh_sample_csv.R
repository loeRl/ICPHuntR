#'@title import_mh_sample_csv
#'@description import CPS and descriptive stats of a signle sample from in the batch folder
#'@param csv_path path to the csv file of the sample
#'@export
#'
#'


import_mh_sample_csv <- function(csv_path = "extdata/200207_MainInc_ME_1_a.b/001SMPL.d/001SMPL_1.csv"){
    mode <- readr::read_delim(csv_path,
                     "/"
                     ,
                     col_names = FALSE,
                     trim_ws = TRUE)


  time_batch <- readr::read_delim(csv_path,
                           ",",
                           col_names = FALSE,
                           trim_ws = TRUE,
                           skip =1)

  (df <- readr::read_csv(csv_path,
                    skip = 7
  ) %>%
      dplyr::filter(stringr::str_detect(Mass, "Printed", T)) %>%
      dplyr::mutate(
        aq.mode = as.character(mode[1,2]),
        aq.time = as.POSIXct(as.character(time_batch[1,2])),
        aq.batch = as.character(time_batch[1,4]),
        file_name =  stringr::str_extract(mode[1,1],"(?<=:)[:graph:]+")
      ) %>%
      dplyr::rename(
        det.mode =X4,
        RSD = "RSD[%]",
        int.time = `Time(Sec)`
      )
  )
  return(df)
}
