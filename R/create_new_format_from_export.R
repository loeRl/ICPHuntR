# create_new_format_from_export
#'@title create_new_format_from_export
#'@description This function creates a gathered tibble with the complete meta-information of a measured sample new columns are: mass,mode,element,parameter,unit,analyte_type
#'@details The function returns an  tibble of a Masshunter Exportsheet (.xlsx imported with read_excel()). The created tibble can be used with all dplyr operations like filter, select, group_by ect. for furher data analyses.
#'the excelsheet should contain the calibration curve or at least the calibration blank measurement set to level1. For further processing, it is advised to wirte the samples name in the Sample name collum and comments in the comment section.
#'@param .df path to a masshunter exportsheet in .xlsx or .xls format
#'@export




create_new_format_from_export <- function(.df = "man/readme/extdata/random_ICP_8900_run.xlsx"){

  analyses <- parameter <- na <- value <- level <- mass <- element <-unit <- analyte_type <- lod <- acq_time <-  NULL # Setting the variables to NULL first

  data.f <- assign_new_names(.df) ## import the datasheet
  data.f_2 <-tidyr::gather(data.f, ## make df longer
                            "analyses",
                            "value",
                            9:length(data.f)
                            )
  data.f_3 <-(data.f_2 %>%
                dplyr::mutate(
                  mass = as.numeric(stringr::str_extract(analyses,"[:digit:]+(?=[:space:])")),
                  mass_shift = as.numeric(stringr::str_extract(analyses, "(?<=-> )\\d+")),
                  mode = stringr::str_trim(
                    stringr::str_extract(analyses,"(?<=\\[).*?(?=\\])")
                    ),
                  element = stringr::str_extract(analyses,"(?<=[:digit:]{1,3}[:blank:]{1,2})[:alpha:]+"),
                  parameter = stringr::str_extract(analyses,"(?<=\\][:blank:]{1,2}).+"),
                  unit = stringr::str_trim(
                    dplyr::if_else(
                      stringr::str_detect(parameter,"RSD"),"%",
                                          dplyr::if_else(
                                            stringr::str_detect(parameter,"CPS"),"CPS",
                                            stringr::str_extract(parameter,"(?<=\\[).*?(?=\\])")
                                            )
                      )
                    ),
                  parameter = stringr::str_trim(
                    stringr::str_remove_all(
                      parameter,c("\\[.*?\\]")
                      )
                    ),
                  analyte_type = dplyr::if_else(
                    stringr::str_detect(analyses,"( ISTD )"),paste("ISTD"),paste("Analyte")
                    )
                )
              )


  names(data.f_3) <- stringr::str_to_lower(
    stringr::str_replace_all(
      stringr::str_remove(
        stringr::str_trim(names(data.f_3)),
        "Sample "),
      " ", "_")
    )

  data.f_4 <- data.f_3 %>%
    dplyr::rename(acq_time="acq._date-time") %>%
    dplyr::select(-analyses,-na)

   data.f_5 <- data.f_4 %>%
     dplyr::filter(parameter == "Conc.") %>%
     dplyr::select(-parameter) %>%
     dplyr::mutate(
      below_lod = stringr::str_detect(value, "<")
    ) %>%
     dplyr::right_join(data.f_4)

  data.f_6 <- dplyr::filter(data.f_5, level == "1") %>%
    dplyr::mutate(
      lod = dplyr::if_else(
        stringr::str_detect(string = value, pattern = "<"),
        as.numeric(stringr::str_remove(value,"<")),
        as.numeric(NA),
        as.numeric(NA))
    ) %>%
    dplyr::select(mass,
                  mode,
                  element,
                  parameter,
                  unit,
                  analyte_type,
                  lod
                  ) %>%
    dplyr::left_join(data.f_5) %>%
    dplyr::mutate(
      value = as.numeric(value),
      acq_time = as.POSIXlt(as.numeric(acq_time)*3600*24, "CET",  "1899-12-30")
    )%>%
    dplyr::select(
    "mass",
    "mass_shift",
    "mode",
    "element",
    "parameter",
    "unit",
    "analyte_type",
    "rjct",
    "data_file",
    "acq_time",
    "type",
    "level",
    "name",
    "vial_number",
    "value",
    "below_lod",
    "lod"
    )



  return(data.f_6)
}
