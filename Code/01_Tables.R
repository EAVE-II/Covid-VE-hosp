##########################################################
# Name of file: 01_Tables.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the risk groups 
#                         runs through tabulations and graphs for vacciantion groups
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_Charlson.rds"))
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_SMR01.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))

df <- EAVE_cohort %>%  
  left_join(rg, by="EAVE_LINKNO")
df <- df %>%  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear))
df <- mutate(df, EAVE_PREGNANCY = factor(case_when(Sex=="M" ~ "No",
                                                   ageYear <= 13 | ageYear >= 54 ~ "No",
                                                   TRUE ~ as.character(EAVE_PREGNANCY))))
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE_BP_Smoke.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO)) %>% 
  select(EAVE_LINKNO,EAVE_Smoking_Status_Worst,EAVE_BP_12M, EAVE_BP)
df <- df %>% 
  left_join(rg, by="EAVE_LINKNO")
rm(rg)


#https://www.gov.uk/government/publications/priority-groups-for-coronavirus-covid-19-vaccination-advice-from-the-jcvi-25-september-2020/jcvi-updated-interim-advice-on-priority-groups-for-covid-19-vaccination#vaccine-priority-groups-interim-advice
#missing neurological conditions
#not poorly controlled diabetes but all
#chronic resp not chronic pulmonary
#no obesity no malignancy
df <- df %>% mutate(vacc_gp_1 = ifelse(EAVE_CARE_HOME=="Yes",1,0),
                    vacc_gp_2 = ifelse(ageYear >= 80 & vacc_gp_1==0,1,0),
                    vacc_1_2 = vacc_gp_1 + vacc_gp_2,
                    vacc_gp_3 = ifelse(ageYear >= 75 & vacc_1_2==0,1,0),
                    vacc_1_3 = vacc_1_2 + vacc_gp_3,
                    vacc_gp_4 = ifelse(ageYear >= 70 & vacc_1_3==0,1,0),
                    vacc_1_4 = vacc_1_3 + vacc_gp_4,
                    vacc_gp_5 = ifelse(ageYear >= 65 & vacc_1_4==0,1,0),
                    vacc_1_5 = vacc_1_4 + vacc_gp_5,
                    high_risk = case_when(EAVE_TRANSPLANT=="Yes" ~ 1,
                                          EAVE_HAEMAT_MALIGNANCY=="Yes" ~ 1,
                                          EAVE_CHRONIC_KIDNEY_DIS=="Yes" ~ 1,
                                          EAVE_IMMUNOSUPPRESSION=="Yes" ~ 1,
                                          EAVE_DEMENTIA =="Yes" ~ 1,
                                          EAVE_STROKE_TIA=="Yes" ~1,
                                          EAVE_DIABETES=="Yes" ~ 1,
                                          EAVE_CHRONIC_RESP_DIS =="Yes" ~1,
                                          EAVE_CHRONIC_LIVER_DIS=="Yes"~1,
                                          EAVE_PER_VASCULAR_DIS=="Yes"~1,
                                          TRUE~0) )
df <- df %>% mutate(vacc_gp_6 = ifelse(ageYear < 65 & high_risk==1 & vacc_1_5==0,1,0),
                    vacc_1_6 = vacc_1_5 + vacc_gp_6,
                    vacc_gp_8 = ifelse(ageYear >= 60  & vacc_1_6==0,1,0),
                    vacc_1_8 = vacc_1_6 + vacc_gp_8,
                    vacc_gp_9 = ifelse(ageYear >= 55  & vacc_1_8==0,1,0),
                    vacc_1_9 = vacc_1_8 + vacc_gp_9,
                    vacc_gp_10 = ifelse(ageYear >= 50  & vacc_1_9==0,1,0),
                    vacc_1_10 = vacc_1_9 + vacc_gp_10  )                              
z.gps <- paste0("vacc_gp_",c(1:6,8,9,10))
z <- df %>% group_by_at(z.gps) %>% 
  dplyr::summarise(N = round(sum(eave_weight))) %>% 
  as.data.frame()
