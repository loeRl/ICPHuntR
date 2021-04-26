unique(mydata) %>% filter(parameter == "Conc.", analyte_type =="Analyte", type == "Sample") %>%
    group_by(mass,mode,element,name) %>%
  summarise(mean_conc =mean(value),
            sd_conc = sd(value),
            n= n()) %>%
  filter(n > 1) %>%
  View()
