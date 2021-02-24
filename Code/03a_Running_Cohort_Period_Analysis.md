
source("01a_Vaccinations_Input.R")
#need to check the format of the vaccination data in case new codes are added

#main output data frames are 
#Vaccinations
#covid_death  # endpoint file
#covid_hospitalisations   # endpoint file
#covid_icu_death   # endpoint file
#df_cohort # risk factors only
#EAVE_Cohort
#Positive_Tests


source("03b_Get_Endpoints_Vaccinations_Long_Period.R")
#set the endpoint selection
#z_event_endpoint <- "hosp" #hosp, death or icu_death

#this calls
source("03b_Vaccinations_Long_Period.R")
#to get the vaccination data for the analysis cohort into long format by
#time from vaccination and week from 8th Dec
#output  is z_vaccination_long
# and z_covariates is the covariate file
#
#then
#
source("03b_Add_Endpoints_To_Vaccinations_Long.R")
#to add the endpoint time to this long format vaccination file
#and add the covariates
#output is 
# df - data frame for the analysis file

source("03b_Cohort_Analysis_Period.R")

#output from this is
#saveRDS(z_res,paste0(project_path,"/output/temp/CR_cohort_summary_",z_event_endpoint,".RDS"))
#CR_cohort_summary_hosp.RDS
#CR_cohort_summary_icu_death.RDS
#CR_cohort_summary_death.RDS

#run markdown files
#02b_Cohort_Results_Hosp.Rmd
#02b_Cohort_Results_ICU.Rmd
#02b_Cohort_Results_Death.Rmd  # not yet made up