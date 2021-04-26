#'
#'
#'@title import_metadata
#'@description the function imports the metadata of all measurements saved in a mashunter batchfile.
#'@param path_batch path to the a target batchfile
#'@export
#'
#'

import_metadata <- function(path_batch = "extdata/200207_MainInc_ME_1_a.b"){
  BatchLog <- readxl::read_xlsx(paste(path_batch,"/BatchLog.xlsx",sep= ""),
                         col_names = c("nr","aq.time","sample.type","sample.name","file.name","comment","vial"),
                        col_types = c("guess", "date","guess","guess","guess","guess","guess"),
                         skip = 1,
                         trim_ws = T) %>%
    dplyr::filter(aq.time >= 0) %>%
    dplyr::mutate(
      file_name = stringr::str_extract(file.name,"(?<=\\.b\\\\)[:graph:]+")
    )
  return(
    BatchLog
  )
}
