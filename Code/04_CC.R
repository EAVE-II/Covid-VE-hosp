##########################################################
# Name of file: 04a_CC.R
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
#                         run 04_CC.R to set up the cc study
#                         matching on Age, Sex
#                         this script fits the models
# Approximate run time: Unknown
##########################################################

#library(Epi)

output_list <- list()

z_event_endpoint <- "death_covid" #hosp, death or icu_death
output_list$endpoint <- z_event_endpoint
output_list$match <- "Specimen Date"   #"event date" "Specimen Date"

#z_test_event is the amount to add onto the speciment data if event time is missing
#not needed
if (z_event_endpoint =="hosp_covid") {z_event <- covid_hospitalisations
                                z_test_event <- 7}
if (z_event_endpoint =="icu_death") {z_event <- covid_icu_death
                                     z_test_event <- 14}
if (z_event_endpoint =="death_covid") {z_event <- covid_death
                                      z_test_event <- 21}

z_event <- z_event %>% mutate(SpecimenDate = if_else(is.na(SpecimenDate) , admission_date - z_test_event, SpecimenDate))

if (output_list$match == "Specimen Date") { #make admission_date the specimen date to match on
  z_event$admission_date_orig <- z_event$admission_date
  z_event$admission_date <- z_event$SpecimenDate
}
a_end <- max(z_event$admission_date)

#get the cohort to sample from - 
#exclude those that are not available to have the endpoint in the future
z_df_cohort <- dplyr::select(EAVE_cohort, EAVE_LINKNO, Sex, ageYear, SpecimenDate, hosp_covid, date_hosp_covid,
                             icu_death, date_icu_death, death_covid, NRS.Date.Death)
z_df_cohort <- z_df_cohort %>% mutate(event = get(z_event_endpoint),
                                      cens_time = NRS.Date.Death) 
if (z_event_endpoint == "death_covid" ) {z_df_cohort <- z_df_cohort %>% 
  mutate(exit_time = ifelse(event==1,NRS.Date.Death, NA) ) %>% 
  mutate(exit_time = as.Date(exit_time, origin=as.Date("1970-01-01"))) } 
if (z_event_endpoint != "death_covid" ) z_df_cohort <- z_df_cohort %>% mutate(exit_time = get(paste0("date_",z_event_endpoint) ) )

z_df_cohort <- z_df_cohort %>%  filter(is.na(exit_time) | !is.na(exit_time)&exit_time > a_begin) %>% 
  dplyr::select(EAVE_LINKNO, Sex, ageYear, SpecimenDate, event, exit_time, cens_time) %>% 
  mutate(exit_time = if_else(is.na(exit_time), a_end, exit_time),
         cens_time = if_else(is.na(cens_time), a_end, cens_time)) %>% 
  mutate(exit_time = if_else(exit_time > cens_time, cens_time,exit_time)) %>% 
  dplyr::select(-cens_time) %>% 
  mutate(start_time= a_begin) %>% 
#  mutate(ageYear = as.character(ageYear)) %>% #make a character for the matching
  filter(exit_time >= a_begin)

#add in the geography
z_df_cohort <- z_df_cohort %>%
  left_join(dplyr::select(df_cohort, EAVE_LINKNO, InterZone, Council), by="EAVE_LINKNO")

#group the ages in the over 80's to make matching easier
z_df_cohort <- z_df_cohort %>% mutate(ageYear = if_else(ageYear >= 100,100, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 95 & ageYear <= 99,97, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 90 & ageYear <= 94,92, ageYear)) %>% 
  mutate(ageYear = if_else(ageYear >= 80 & ageYear <= 89, trunc(ageYear/2)*2, ageYear)) 


#merge in n_risk_gps and n_tests_gp for matching
z_df_cohort <- z_df_cohort %>% 
  left_join(dplyr::select(df_cohort, EAVE_LINKNO, n_risk_gps, n_tests_gp), by="EAVE_LINKNO")
z_df_cohort <- z_df_cohort %>%   filter(!is.na(n_risk_gps))

z_cases <- z_df_cohort %>% filter(event==1)
z_cases <- z_cases %>% mutate(SpecimenDate = if_else(is.na(SpecimenDate) , exit_time - z_test_event, SpecimenDate))
if (output_list$match == "Specimen Date") { #remove specimens before a_begin
  z_cases <- filter(z_cases, SpecimenDate > a_begin)
}
#z_cont <- slice_sample(z_df_cohort, n=nrow(z_cases)*1000) #to speed things up
#omit previously positive from potential controls
z_cont <- z_df_cohort %>% filter(is.na(SpecimenDate) | (!is.na(SpecimenDate) & (SpecimenDate > a_begin)) ) 
#z_cc_data <- bind_rows(z_cases,z_cont)
#set.seed(20140111)
## Generate a nested case-control study
#z_cc <- ccwc(entry    = start_time,    # Time of entry to follow-up
#               exit     = exit_time,    # Time of exit from follow-up
#               fail     = event,    # Status on exit (1 = Fail, 0 = Censored)
#               origin   = start_time,    # Origin of analysis time scale
#               controls = 1,      # The number of controls to be selected for each case
#               data     = z_df_cohort,   # data frame
#               include  = EAVE_LINKNO, # List of other variables to be carried across into the case-control study
#               match    = list(ageYear, Sex),    # List of categorical variables on which to match cases and controls
#               silent   = FALSE
#)

#for the matched case control study
#do it by merging and selecting 10 controls
z_merge <- left_join(z_cont, z_cases, by=c("ageYear","Sex", "n_risk_gps", "n_tests_gp"), suffix=c("_cont","_case")) %>% 
  filter(!is.na(EAVE_LINKNO_case)) %>% 
  filter(EAVE_LINKNO_case != EAVE_LINKNO_cont)

if (output_list$match == "event date") { #match on date of event
  z_merge <- z_merge %>%  filter(exit_time_cont > exit_time_case)  
}
if (output_list$match == "Specimen Date") { #match on date of sample
  z_merge <- z_merge %>%  filter(exit_time_cont > SpecimenDate_case)  
}

#select 10 here
z_merge_10 <- z_merge %>%  mutate(random_id = runif(nrow(z_merge))) %>% 
  arrange(random_id) %>% #to make sure that the same controls are not always selected
  group_by(EAVE_LINKNO_case) %>% 
  mutate(id = row_number()) %>% ungroup() %>% 
  filter(id <= 10) %>% arrange(EAVE_LINKNO_case) 

#table(table(z_merge_10$EAVE_LINKNO_cont)) #duplicate controls
#table(table(z_merge_10$EAVE_LINKNO_case)) # controls per case
#table(z_merge_10$event_cont) # controls could be cases in future
z <- filter(z_merge_10, event_cont==1)
table(z$exit_time_cont > z$exit_time_case, exclude=NULL)
table(z$SpecimenDate_cont > z$SpecimenDate_case, exclude=NULL)

#cases not matched
z_sel <- !(z_cases$EAVE_LINKNO %in% unique(z_merge_10$EAVE_LINKNO_case))
table(z_sel)
z_no_match <- filter(z_cases, z_sel)
#elderly do not always match

#put the cases and controls together
z_cc_case <- z_merge_10 %>% dplyr::select(Sex, ageYear, n_risk_gps, n_tests_gp, SpecimenDate_case, event_case, exit_time_case, EAVE_LINKNO_case) %>% 
  filter(!duplicated(EAVE_LINKNO_case)) %>% 
  mutate(EAVE_LINKNO = EAVE_LINKNO_case, SpecimenDate=SpecimenDate_case) %>% 
  relocate(EAVE_LINKNO, .before=Sex) %>% 
  dplyr::rename(event=event_case, event_time = exit_time_case) 
z_cc_cont <- z_merge_10 %>% dplyr::select(EAVE_LINKNO_cont, Sex, ageYear, n_risk_gps, n_tests_gp, SpecimenDate_case, event_cont, exit_time_cont, EAVE_LINKNO_case, SpecimenDate_cont) %>% 
  dplyr::rename(EAVE_LINKNO = EAVE_LINKNO_cont, event=event_cont, 
                event_time = exit_time_cont, SpecimenDate = SpecimenDate_cont ) %>% 
  mutate(event=if_else(event==1,0,event)) 
#some controls will become cases in the future - set event ==0 for them 
#but keep the exit time as the time of the event

print(nrow(z_cc_case))

z_cc <- bind_rows(z_cc_case, z_cc_cont) %>% 
  mutate(ageYear = as.numeric(ageYear)) %>% 
  arrange(EAVE_LINKNO_case, desc(event))  
#  filter(SpecimenDate_case >= a_begin - 28) #omit cases from a long time ago
print(table(z_cc$event))

#merge to the vaccination data but only use vaccinations up to the last event data
#make anyone vaccinated after the maximum endpoint time unvaccinated
z_vaccinations <- filter(Vaccinations, date_vacc_1 < a_end) %>% 
  mutate(vacc_type_2 = if_else(date_vacc_2 >= a_end, NA_character_ , vacc_type_2),
         date_vacc_2 = as.Date(ifelse(date_vacc_2 >= a_end, NA, date_vacc_2), origin=as.Date("1970-01-01")) )

#using the time of admission to hosp/icu/death

df_cc <- z_cc %>% left_join(z_vaccinations, by="EAVE_LINKNO") %>%
  mutate(day_1 = as.numeric(event_time - date_vacc_1),
         day_2 = as.numeric(event_time - date_vacc_2)) %>% 
  mutate(vacc_1_gp = cut(day_1, breaks= c((min(day_1, na.rm=T)-1), 0, 6, 13, 20, 27, 34, 41, max(day_1, na.rm=T)),
                         labels=c("uv","v1_0:6","v1_7:13","v1_14:20","v1_21:27","v1_28:34","v1_35:41", "v1_42+")),
         vacc_2_gp = cut(day_2, breaks= c(-100, 0, 6,  max(day_2, na.rm=T)) ,
                         labels=c("v2_uv" , "v2_0:6","v2_7+")) ) %>% 
  mutate(vacc_status = as.character(vacc_1_gp)) %>% 
  mutate(vacc_status = case_when( is.na(date_vacc_1) ~ "uv",
                                  !is.na(vacc_2_gp) &(vacc_2_gp %in% c("v2_0:6","v2_7+")) ~ as.character(vacc_2_gp),
                                  TRUE ~ vacc_status)) %>% 
  mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_0:6","v1_7:13","v1_14:20",
                                                      "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) )

table(df_cc$vacc_1_gp, df_cc$vacc_2_gp, exclude=NULL)
table(df_cc$vacc_status, df_cc$event, exclude=NULL)
table(df_cc$event, exclude=NULL)

df_cc <- df_cc %>% dplyr::select(-(date_vacc_1:vacc_2_gp))
  
df_cc <- df_cc %>% left_join(dplyr::select(df_cohort, -Sex, -ageYear, -n_risk_gps, -n_tests_gp ), by="EAVE_LINKNO")

#test_before_dec8 is individual based - need to change it so that
#all controls are omitted if the case had a test beforehand
z <- df_cc %>%  dplyr::select(EAVE_LINKNO, test_before_dec8) %>% 
  filter(EAVE_LINKNO %in% z_cases$EAVE_LINKNO) %>% 
  filter(!duplicated(EAVE_LINKNO))
z1 <- df_cc %>% left_join(z, by=c("EAVE_LINKNO_case" = "EAVE_LINKNO"), suffix=c("","_case"))
z1 <- z1 %>% mutate(tt = test_before_dec8) %>% 
  mutate(tt = if_else(event==0 &!(test_before_dec8_case %in% c("post-vacc", "no pos test")), test_before_dec8_case, tt)) %>% 
  mutate(test_before_dec8 = tt) %>% 
  dplyr::select(-tt, -test_before_dec8_case)
df_cc <- z1

#add in the vacccination type - already in add value for uv and get rid of vacc type 2 as it is the same
df_cc <- df_cc %>% mutate(vacc_type = if_else(is.na(vacc_type), "uv", vacc_type)) %>% 
  dplyr::select(-vacc_type_2)

#add in the hospitalisation status at specimen date
#little point in doing this at the date of death
#get all admissions in the period 14 days before to get historical adm
#cc indivs plus specimen date of case for timing
z_cc <- df_cc %>% filter(!duplicated(EAVE_LINKNO)) %>% 
  dplyr::select(EAVE_LINKNO, SpecimenDate_case)

z_hosp <- hosp_adm_nov01 %>% filter (is.na(discharge_date) | discharge_date >= a_begin-14) %>% 
  filter(EAVE_LINKNO %in% z_cc$EAVE_LINKNO) #omit individuals not in the selected data

z_cc_h <- left_join(z_cc, z_hosp, by="EAVE_LINKNO") %>% 
  mutate(hosp = case_when(is.na(admission_date) & is.na(discharge_date) ~ "no_adm",
    !is.na(discharge_date) & (discharge_date < SpecimenDate_case-14) ~ "no_adm",
    !is.na(discharge_date) & (discharge_date >= SpecimenDate_case-14) & (discharge_date < SpecimenDate_case) ~ "before",
    !is.na(discharge_date) & (discharge_date >= SpecimenDate_case) & !is.na(admission_date) & (admission_date <= SpecimenDate_case) ~ "in_hosp",
    !is.na(admission_date) & is.na(discharge_date) & (admission_date <= SpecimenDate_case)  ~ "in_hosp",
    !is.na(admission_date) & (admission_date > SpecimenDate_case)  ~ "after",
    TRUE ~ "check") ) %>% 
  mutate(value=1)
z_cc_h_indiv <- z_cc_h %>% dplyr::select(-(SpecimenDate_case:discharge_date)) %>% 
  pivot_wider(id_cols=EAVE_LINKNO, names_from=hosp, values_from=value, values_fill = 0 , values_fn=sum) %>% 
  mutate(hosp = case_when(in_hosp > 0 ~ "in_hosp",
                          in_hosp==0 & before > 0  ~ "before",
                          TRUE ~ "no_adm"))
 
df_cc <- df_cc %>% left_join(dplyr::select(z_cc_h_indiv,EAVE_LINKNO, hosp ), by="EAVE_LINKNO") 
df_cc$hosp <- factor(df_cc$hosp, levels=c("no_adm","before","in_hosp"))
table(df_cc$hosp)
table(df_cc$event)
table(df_cc$hosp, df_cc$event)
table(df_cc$hosp, df_cc$event)
table(df_cc$vacc_status, df_cc$event, df_cc$hosp)

saveRDS(df_cc, paste0(project_path,"/output/temp/df_cc_",z_event_endpoint,".RDS"))


