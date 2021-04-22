##########################################################
# Name of file: 05_Match_Vacc.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 21 Jan 2021
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: run 01a_Vaccinations_Input.R first
#                         matching on Age, Sex plus - to change the variables the list has to be changed in a number of places
#                         this script fits the models
# Approximate run time: Unknown
##########################################################

#library(Epi)

output_list <- list()
output_list$prev_pos <- "remove"
z_event_endpoint <- "hosp_covid" #hosp, death or icu_death
output_list$event <- z_event_endpoint
#z_test_event is the amount to add onto the speciment data if event time is missing
#not needed
if (z_event_endpoint =="hosp_covid") {z_event <- covid_hospitalisations
                                z_test_event <- 7
                                z_main <- "Hospitalisation"}
if (z_event_endpoint =="icu_death") {z_event <- covid_icu_death
                                     z_test_event <- 14
                                     z_main="Covid Death/ICU"}
if (z_event_endpoint =="death_covid") {z_event <- covid_death
                                 z_test_event <- 21
                                 z_main="Covid Death"}

a_end <- max(z_event$admission_date)
z_event <- z_event %>% mutate(SpecimenDate = if_else(is.na(SpecimenDate), admission_date - z_test_event, SpecimenDate))

#make anyone vaccinated after the maximum endpoint time unvaccinated
z_vaccinations <- filter(Vaccinations, date_vacc_1 < a_end) %>% 
  mutate(vacc_type_2 = if_else(date_vacc_2 >= a_end, NA_character_ , vacc_type_2),
         date_vacc_2 = as.Date(ifelse(date_vacc_2 >= a_end, NA, date_vacc_2), origin=as.Date("1970-01-01")) )

#get the vaccinated individuals in the time period 
z_df <- dplyr::select(z_vaccinations, EAVE_LINKNO, date_vacc_1) %>% 
  left_join(dplyr::select(df_cohort, EAVE_LINKNO, Sex, ageYear, Council, n_risk_gps), by="EAVE_LINKNO") %>% 
  filter(!is.na(ageYear)) %>% 
  filter(date_vacc_1 <= as.Date("2021-02-14"))
#merge in the event file and remove any who are vaccinated after their event - there will not be many
z_df <- z_df %>% left_join(z_event, by="EAVE_LINKNO")
z_df <- z_df %>% filter(is.na(admission_date) | admission_date > date_vacc_1)
z_df <- z_df %>% dplyr::rename(EAVE_LINKNO_vacc = EAVE_LINKNO) %>% 
  dplyr::select(-(SpecimenDate:admission_date))

#group the ages in the over 80's to make matching easier
z_df <- z_df %>% mutate(ageYear = if_else(ageYear >= 100,100, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 95 & ageYear <= 99,97, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 90 & ageYear <= 94,92, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 80 & ageYear <= 89, trunc(ageYear/2)*2, ageYear)) 

#this is the whole population to link in with
z_chrt <- df_cohort %>% dplyr::select(EAVE_LINKNO, Sex, ageYear, Council, n_risk_gps) %>% 
  dplyr::rename(EAVE_LINKNO_uv = EAVE_LINKNO) %>% 
  mutate(ageYear = if_else(ageYear >= 100,100, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 95 & ageYear <= 99,97, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 90 & ageYear <= 94,92, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 80 & ageYear <= 89, trunc(ageYear/2)*2, ageYear))

#removal of vaccinated and potential control previously positive
if (output_list$prev_pos == "remove") {
  z_df <- z_df %>% 
    left_join(dplyr::select(df_cohort, EAVE_LINKNO, test_before_dec8), by=c("EAVE_LINKNO_vacc" = "EAVE_LINKNO")) %>% 
    filter(test_before_dec8 %in% c("no pos test","post-vacc")) %>% 
    dplyr::select(-test_before_dec8)

  z_chrt <- z_chrt %>% 
    left_join(dplyr::select(df_cohort, EAVE_LINKNO, test_before_dec8), by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO")) %>% 
    filter(test_before_dec8 %in% c("no pos test","post-vacc")) %>% 
    dplyr::select(-test_before_dec8)
  }

#use only unvaccinated controls whose eave_weight is high - to counteract individuals who don't exist
#made no difference
#z_id <- filter(df_cohort, eave_weight >= 0.95)$EAVE_LINKNO
#z_chrt <- filter(z_chrt, EAVE_LINKNO_uv %in% z_id )



#merge the vaccinated with everyone - someone who is vaccinated can be a match for an earlier vaccinated person
z_merge <- z_df %>% left_join(z_chrt, by=c("Sex","ageYear","Council","n_risk_gps")) #merge by the matching variables
#merge in the vaccination dates of the potential matches
z_merge <-  z_merge %>% 
  left_join(dplyr::select(z_vaccinations, EAVE_LINKNO, date_vacc_1),
            by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO"), suffix=c("","_uv") )
#remove any matched where the date of vaccination of the control is before the vaccinated case
z_merge <- z_merge %>% filter(date_vacc_1 < date_vacc_1_uv | is.na(date_vacc_1_uv))
#merge in the events to select out any matched unvaccinated with event before the vaccinated case was vaccinated
#these are ineligible for matching
z_merge <- z_merge %>% left_join(z_event, by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO"))
z_merge <- z_merge %>% filter(is.na(admission_date) | admission_date > date_vacc_1) %>% 
  dplyr::select(-(SpecimenDate:admission_date))
#merge in all deaths to select out any matched unvaccinated who died before the vaccinated case was vaccinated
#ineligible for matching
z_merge <- z_merge %>% left_join(any_death, by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO"))
z_merge <- z_merge %>% filter(is.na(admission_date) | admission_date > date_vacc_1) %>% 
  dplyr::select(-(SpecimenDate:admission_date))
#add a random id to sort on  
z_merge <- z_merge %>% mutate(random_id = runif(nrow(z_merge))) %>% 
  arrange(random_id) %>% #randomly rearrange to make sure that the same controls are not always selected
  filter(!duplicated(EAVE_LINKNO_vacc)) %>%  #get one match per vaccinated individuals
  dplyr::select(-random_id)

#put the cases and controls together 
z_cc_vacc <- z_merge %>% dplyr::select(Sex, ageYear, Council, n_risk_gps, EAVE_LINKNO_vacc, date_vacc_1, date_vacc_1_uv) %>% 
  mutate(EAVE_LINKNO = EAVE_LINKNO_vacc) %>% 
  relocate(EAVE_LINKNO, .before=Sex) %>% 
  dplyr::rename(date_vacc_1_vacc = date_vacc_1) 
  
z_cc_uv <- z_merge %>% dplyr::select(EAVE_LINKNO_uv, Sex, ageYear, Council, n_risk_gps, EAVE_LINKNO_vacc, date_vacc_1, 
                                     date_vacc_1_uv) %>% 
  dplyr::rename(EAVE_LINKNO = EAVE_LINKNO_uv, date_vacc_1_vacc = date_vacc_1) 
  
#some unvaccinated controls will become vaccinated  in the future - keep the date they get vaccinated

z_cc <- bind_rows(z_cc_vacc, z_cc_uv) %>% 
  arrange(EAVE_LINKNO_vacc) %>% 
  mutate(vacc = factor(if_else(EAVE_LINKNO==EAVE_LINKNO_vacc, 1,0), levels=c(0,1),labels=c("uv","vacc")))
#  filter(SpecimenDate_case >= a_begin - 28) #omit cases from a long time ago
# save a copy of the matched data before adding in any endpoints
saveRDS(z_cc, paste0(project_path,"/output/temp/matched_vacc_unvacc_cc_14022021.RDS"))

#start here when reading in the data - need to run 01_Vaccination_input.R first
#z_cc <- readRDS(paste0(project_path,"/output/temp/matched_vacc_unvacc_cc_14022021.RDS"))

#using the time of admission to hosp/icu/death to set up the event time
z_cc <- z_cc %>% mutate(event_date = a_end) %>%
  #relocate the follow up to the date the unvaccinated contrtol was vaccinated
  mutate(event_date = if_else(!is.na(date_vacc_1_uv) & (date_vacc_1_uv < event_date), date_vacc_1_uv, event_date))

#merge in the event date to calculate the event and event date
z_cc <- z_cc %>% left_join(z_event, by="EAVE_LINKNO")
z_cc <- z_cc %>% mutate(event = if_else(!is.na(admission_date), 1,0)) %>% 
  mutate(event_date = if_else( (event==1) & (admission_date <= event_date), admission_date, event_date))
#change the event marker from 1 to 0 for those whose admission_date is greater than the current event_date
#these are instances where the unvaccinated control is vaccinated before the admission_date
#and hence censored at the vaccination date
z_cc <- z_cc %>%  mutate(event = if_else(event==1 & (admission_date > event_date), 0, event))
z_cc <- z_cc %>%  dplyr::select(-(SpecimenDate:admission_date))

#link in any death and modify the event date.  There should not be any events to change as event date <= death date.  
z_cc <- z_cc %>% left_join(any_death, by="EAVE_LINKNO")
z_cc <- z_cc %>% 
  mutate(event_date = if_else(!is.na(admission_date) & (admission_date < event_date), admission_date, event_date))
z_cc <- z_cc %>%  dplyr::select(-(SpecimenDate:admission_date))


#calculate the time on study
z_cc <- z_cc %>%  mutate(time_to_hosp = as.numeric(event_date-date_vacc_1_vacc))

#time on study possible negative for data errors - omit both vacc and matched unvacc
z <- filter(z_cc, time_to_hosp <0)
z <- filter(z_cc, EAVE_LINKNO_vacc %in% z$EAVE_LINKNO_vacc)
z_cc <- filter(z_cc, !(EAVE_LINKNO_vacc %in% z$EAVE_LINKNO_vacc))

#link to vaccination type - link both vaccinated and unvaccinated control so that
#selection on vacc type can be easily made.
z_cc <- z_cc %>%
  left_join(dplyr::select(z_vaccinations, EAVE_LINKNO, vacc_type), by=c("EAVE_LINKNO_vacc" = "EAVE_LINKNO") )

df_cc <- z_cc %>% 
  left_join(dplyr::select(df_cohort, -(Sex:ageYear), -Council, -n_risk_gps ), by="EAVE_LINKNO") 


