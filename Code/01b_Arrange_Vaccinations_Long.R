##########################################################
# Name of file: 01b_Arrange_Vaccinations_Long.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 20 Jan 2021
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: run 01_Vaccinations_Input.R first
#                         arranges the vaccinations in long format first
# Approximate run time: Unknown
##########################################################



#Get Vaccination data into a long format
 
#a_begin <- as.Date("2020-12-08") in 01a
a_end <- max(covid_hospitalisations$admission_date)
#z_vacc <- slice_sample(Vaccinations, n=100)

z_n_rows <- nrow(Vaccinations)
z_n_gps <- ceiling(z_n_rows/200000) 
z_seq <- round(seq(0,z_n_rows, length.out=z_n_gps))

rm(z_wide_store)
for (i in 1:(z_n_gps-1)) { 
  #i <- 3
print(i)
z_vacc <- Vaccinations[(z_seq[i] + 1) : z_seq[(i+1)] , ]

z_days <- data.frame(Date=seq.Date(a_begin,a_end, by="days"))
z_id <- z_vacc %>% dplyr::select(EAVE_LINKNO, date_vacc_1, date_vacc_2)

z <- full_join(z_id, z_days, by=character())

z <- z %>%   mutate(day_1 = as.numeric(Date - date_vacc_1),
                    day_2 = as.numeric(Date - date_vacc_2)) %>% 
  mutate(vacc_1_gp = cut(day_1, breaks= c((min(day_1, na.rm=T)-1), 0, 6, 13, 20, 27, 34, 41, max(day_1, na.rm=T)),
                         labels=c("uv","v1_0:6","v1_7:13","v1_14:20","v1_21:27","v1_28:34","v1_35:41", "v1_42+")),
         vacc_2_gp = cut(day_2, breaks= c((min(day_2, na.rm=T)-1), 0, 6,  max(day_2, na.rm=T)) ,
                         labels=c("v2_uv" , "v2_0:6","v2_7+")) ) %>% 
  mutate(vacc_status = as.character(vacc_1_gp)) %>% 
  mutate(vacc_status = case_when( !is.na(vacc_2_gp)&vacc_2_gp != "v2_uv"  ~ as.character(vacc_2_gp),
                                  TRUE ~ vacc_status)) %>% 
  mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_0:6","v1_7:13","v1_14:20",
                                                      "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) )

z_wide <- z %>% mutate(date = lubridate::ymd(Date)) %>% 
  group_by(EAVE_LINKNO, vacc_status) %>% 
  dplyr::summarise(start = min(date), stop=max(date)) %>% ungroup()

z_wide_store <- if(exists("z_wide_store")) bind_rows(z_wide_store, z_wide) else z_wide
}

z_wide <- z_wide_store
rm(z_wide_store, z,z_days, z_id, z_vacc) 
z_wide <- z_wide %>% filter(EAVE_LINKNO %in% EAVE_cohort$EAVE_LINKNO)

# z_wide has the stop start intervals for the vaccinated
#  now get the start (a_begin) and stop (a_end) for the unvaccinated

z_unvacc <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO) %>% 
  filter(!(EAVE_LINKNO %in% unique(z_wide$EAVE_LINKNO)))
z_unvacc <- z_unvacc %>% mutate(start = a_begin, stop=a_end,
                                vacc_status = "uv") %>% 
  mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_0:6","v1_7:13","v1_14:20",
                                                      "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) )

z <- bind_rows(z_wide, z_unvacc)

saveRDS(z, paste0(project_path,"/output/temp/vaccination_long_no_period.RDS"))

rm(z, z_days, z_id, z_unvacc, z_vacc, z1)
rm(z_wide)

