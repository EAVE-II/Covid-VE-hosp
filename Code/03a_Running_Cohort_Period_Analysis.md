
source("01a_Vaccinations_Input.R")
#need to check the format of the vaccination data in case new codes are added

#main output data frames are 
#Vaccinations
#covid_death  # endpoint file
#covid_hospitalisations   # endpoint file
#covid_icu_death   # endpoint file
#any_death # endpoint file but used to curtail time at risk
#df_cohort # risk factors only
#EAVE_Cohort
#Positive_Tests
#variables_hosp  - needed for the full adjustment in 03b_Cohort_Analysis_Period.R


source("03b_Get_Endpoints_Vaccinations_Long_Period.R")
#set the endpoint selection
#z_event_endpoint <- "hosp_covid" #"hosp_covid, death_covid, icu_death, any_death

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
# this is saved out as rds file 
#/output/temp/df_hosp_covid.RDS, /output/temp/df_death_covid.RDS, /output/temp/df_icu_death.RDS

source("03b_Cohort_Analysis_Period.R")
#run once with with each setting for prop_score
#output_list$prop_score <- "inverse propensity weighting" 
#"no propensity weighting"  "no propensity weighting"

#the excel file 02_data_selection_flags.csv
#controls the selections for the various models fitted.  id needs to be unique but not sequential

#output from this is
#z_ipw <- if (output_list$prop_score=="inverse propensity weighting") "ipw" else "adj"
#z_fname <- paste0(project_path,"/output/temp/CR_cohort_",z_ipw,"_",z_event_endpoint,".RDS")

#CR_cohort_ipw_hosp_covid.RDS
#CR_cohort_adj_hosp_covid.RDS
# similar files for the other endpoints

#run markdown files
#03c_Cohort_Results_Paper.Rmd  - this reads in both results files for an endpoint 
#and combines the results from all 3 models
#03c_Cohort_Results.Rmd - you need to read in the appropriate results file
#in both cases you need to edit the file to pick up the appropriate results files.

