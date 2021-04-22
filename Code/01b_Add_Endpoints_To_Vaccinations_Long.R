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

z <- readRDS(paste0(project_path,"/output/temp/vaccination_long_no_period.RDS"))

#z <- Vaccinations
z_event_endpoint <- "hosp" #hosp, death or icu_death

if (z_event_endpoint =="hosp") z_event <- covid_hospitalisations
if (z_event_endpoint =="icu_death") z_event <- covid_icu_death
if (z_event_endpoint =="death") z_event <- covid_death

a_end <- max(z_event$admission_date)
#in z_event admission date is generic and meansdate death for deaths, date icu admission for icu

z <- z %>% left_join(z_event, by="EAVE_LINKNO")
df <- z %>% mutate(event = ifelse(is.na(admission_date), 0, 1))
print(length(unique(df$EAVE_LINKNO[df$event==1]))) #check numebr of events

#remove time periods post event time (admission_date)
#keep all with no event and the admission date after the start
#change event if admission after the end of the period (stop)
df <- df %>% filter(event==0 | (event==1)&(start < admission_date)) %>% 
  mutate(event = if_else(event==1 &(admission_date > stop), 0, event)) %>% 
  mutate(stop = if_else(event==1 & (admission_date <= stop) , admission_date, stop )) %>% 
  filter(!((start==stop) & event==0))
xx <- table(filter(df,event==1)$EAVE_LINKNO)
xx <- xx[xx>1]
print(xx) # any with more than 0ne event
length(unique(filter(df,event==1)$EAVE_LINKNO))
#z_event$EAVE_LINKNO[!( z_event$EAVE_LINKNO %in% unique(filter(df,event==1)$EAVE_LINKNO) )]

#add in all deaths and remove periods post death - there should not be any for events
if (z_event_endpoint != "any_death") { 
  #change the name back from the generic
  z_any_death <- any_death %>% dplyr::rename(NRS.Date.Death = admission_date) %>% 
    dplyr::select(-SpecimenDate)
  z <- df %>% left_join(z_any_death, by="EAVE_LINKNO")
  z <- z %>% filter(is.na(NRS.Date.Death) | !is.na(NRS.Date.Death) & (start < NRS.Date.Death) ) %>% 
    mutate(stop = if_else(event==0 & !is.na(NRS.Date.Death) & (NRS.Date.Death < stop) , NRS.Date.Death, stop ))  
  df <- z %>% dplyr::select(-NRS.Date.Death)
}

df <- df %>% mutate(start = as.numeric(start-a_begin),
                    stop= as.numeric(stop-a_begin) ) %>% 
  dplyr::select(-admission_date)

df <- df %>% 
  left_join(dplyr::select(EAVE_cohort, EAVE_LINKNO, ageYear, Sex, simd2020_sc_quintile, eave_weight),
            by="EAVE_LINKNO")

z <- dplyr::filter(Positive_Tests, specimen_date < a_begin-28)
df <- df %>% filter(!(EAVE_LINKNO %in% z$EAVE_LINKNO))


#merge in risk groups

z <- df %>% left_join(rg, by="EAVE_LINKNO")
z <- z %>% filter(!is.na(EAVE_ASTHMA)) # not sure why there is a missing

df <- z

rm(z, z_event)

saveRDS(df, paste0(project_path,"/output/temp/df_",z_event_endpoint,".RDS"))
