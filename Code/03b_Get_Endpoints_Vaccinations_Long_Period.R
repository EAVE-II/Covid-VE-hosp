##########################################################
# Name of file: 03b_Get_Endpoints_Vaccinations_Long_Period.R
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
#                         gets all the cases and 100 controls for each case
#                         arranges a vaccine 
# Approximate run time: Unknown
##########################################################
output_list <- list()
z_event_endpoint <- "hosp_covid" #hosp, death or icu_death
if (z_event_endpoint =="hosp_covid") z_event <- covid_hospitalisations
if (z_event_endpoint =="icu_death") z_event <- covid_icu_death
if (z_event_endpoint =="death_covid") z_event <- covid_death
if (z_event_endpoint =="any_death") z_event <- any_death

a_end <- max(z_event$admission_date)
output_list$endpoint <- z_event_endpoint
output_list$number_events <- nrow(z_event)
output_list$last_event <- a_end
z_n_controls_per_event <- 50
output_list$controls_per_event <- z_n_controls_per_event

#make anyone vaccinated after the maximum endpoint time unvaccinated
z_vaccinations <- filter(Vaccinations, date_vacc_1 < a_end) %>% 
  mutate(vacc_type_2 = if_else(date_vacc_2 >= a_end, NA_character_ , vacc_type_2),
              date_vacc_2 = as.Date(ifelse(date_vacc_2 >= a_end, NA, date_vacc_2), origin=as.Date("1970-01-01")) )

#in z_event admission date is generic and meansdate death for deaths, date icu admission for icu

print(z_event_endpoint)
print(nrow(z_event))
#investigation of numbers and timings - events among vaccinated
#join events to vaccinations
z1 <- left_join(z_event, z_vaccinations, by="EAVE_LINKNO") %>% 
  mutate(vaccinated = if_else(!is.na(date_vacc_1), 1,0)) %>% 
  mutate(vacc_at_event = if_else(!is.na(date_vacc_1) & date_vacc_1 > admission_date, 0, vaccinated) )
table(z1$vaccinated, z1$vacc_at_event, exclude=NULL)


z_event_ids <- unique(z_event$EAVE_LINKNO)
z_rest <- filter(df_cohort, !(EAVE_LINKNO %in% z_event_ids))
z_ids_rest <- unique(z_rest$EAVE_LINKNO)
z_ids_rest <- sample(z_ids_rest, size=nrow(z_event)*z_n_controls_per_event, replace=FALSE)
z_ids <- c(z_event_ids, z_ids_rest)
z_df <- filter(df_cohort, EAVE_LINKNO %in% z_ids )

#z_ids are the ids of those in tha analysis (all cases and 100 controls)
#z_df is the covariates for these individuals

#now get the vaccinations in long format with period

source("03b_Vaccinations_Long_Period.R")
table(z_vaccination_long$vacc_status, exclude=NULL)

#result is z_vaccination_long
#z_covariates is the covariate file

#add in the event end point

source("03b_Add_Endpoints_To_Vaccinations_Long.R")

#df is the data frame for analysis
#check the numbers
print(table(df$event))
print(nrow(z_event))
print(table(df$vacc_status, df$event))
print(sum(df$vacc_status != "uv" & df$event==1))

#add in the vacccination type
df <- df %>% left_join(dplyr::select(Vaccinations, EAVE_LINKNO, vacc_type) , by="EAVE_LINKNO") %>% 
  mutate(vacc_type = if_else(is.na(vacc_type), "uv", vacc_type))

df <- df %>% mutate(weight = if_else(event==1,1,nrow(df_cohort)/(z_n_controls_per_event*nrow(z_event)) ) )  %>% 
  mutate(weight = if_else(event==1,1, eave_weight*weight) ) 

saveRDS(df, paste0(project_path,"/output/temp/df_",z_event_endpoint,".RDS"))
