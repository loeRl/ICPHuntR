mydata <- ICPHuntR::create_new_format_from_export()


mydata$value <- round(mydata$value,4)

#### Calculate the detection limit of a run

end_cal_1 <- mydata %>%
  dplyr::filter(type == "CalStd")
end_cal_2 <- max(end_cal_1$acq_time)
### get timepoint of the last cal point
lods_loqs <- mydata %>%
dplyr::filter(parameter == "Conc.",
              type == "BlkVrfy",
         acq_time >= end_cal_2) %>% ### filter out all blank measurements that appear after the calibration curve
  dplyr::group_by(mass,mass_shift, element, unit) %>%
  dplyr::summarize(LOD = stats::sd(value, na.rm = T)*3,
                   LOQ = stats::sd(value, na.rm = T)*10,
                   ) %>%
  dplyr::arrange(mass)


#### Calculate calculate recoveries of different

qc <- mydata %>%
  dplyr::filter(stringr::str_detect(type,"QC"),parameter == "Conc.")
cal <- mydata %>%
  dplyr::filter(stringr::str_detect(type,"Cal"),parameter == "Conc.")
qc_cal <- qc %>% dplyr::semi_join(cal, by = "vial_number")
qc_ref <- qc %>% dplyr::anti_join(qc_cal, by = "vial_number")

data("standardmaterials")

remanes_std <- standardmaterials %>%
  dplyr::rename(name = reference_material)

qc_recoveries <- dplyr::left_join(qc_ref, remanes_std, by= c("element", "name"),suffix = c(".data",".reference")) %>%
  dplyr::mutate(
    revocery = round(value.data/value.reference*100,1),
    in_range = dplyr::if_else((value.data >= (value.reference-uncert)&(value.data <= (value.reference+uncert))),T,F)
  ) %>%
  dplyr::select(
    name,element,value.data,unit.data,value.reference,uncert,unit.reference,revocery,in_range
  ) %>%
  dplyr::filter(!is.na(revocery)) %>%
  dplyr::arrange(name,revocery)
View(qc_recoveries)
