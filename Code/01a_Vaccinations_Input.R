##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 20 Jan 2021
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in vaccination data 
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

project_path <- paste0(Location,"EAVE/GPanalysis/progs/CR/Vaccine")

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-02-16.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))

table(EAVE_cohort$death_covid, is.na(EAVE_cohort$NRS.Date.Death), exclude=NULL)
table(EAVE_cohort$icu_death, is.na(EAVE_cohort$date_icu_death), exclude=NULL)
table(EAVE_cohort$hosp_covid, is.na(EAVE_cohort$date_hosp_covid), exclude=NULL)


a_begin <- as.Date("2020-12-08")
#remove all who have died before the beginning
EAVE_cohort <- filter(EAVE_cohort, is.na(NRS.Date.Death) | (!is.na(NRS.Date.Death) & NRS.Date.Death > a_begin))
#remove under 18s
EAVE_cohort <- filter(EAVE_cohort, ageYear >= 18)

EAVE_Weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
EAVE_cohort  <- EAVE_cohort %>% left_join(EAVE_Weights, by="EAVE_LINKNO")
EAVE_cohort$eave_weight[is.na(EAVE_cohort$eave_weight)] <- mean(EAVE_cohort$eave_weight, na.rm=T)

#adjust inconsistencies in the endpoints and times - all hosp have an admission date
z_max_date_death <- max(EAVE_cohort$NRS.Date.Death, na.rm=T)
z_max_date_icu <- max(EAVE_cohort$date_icu_death, na.rm=T)
#EAVE_cohort <- EAVE_cohort %>% mutate(NRS.Date.Death = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ SpecimenDate + 21,
#                                                       TRUE ~ NRS.Date.Death),
#                            date_icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ SpecimenDate + 14,
#                                                       TRUE ~ date_icu_death ) ) %>% 
#  mutate(NRS.Date.Death = case_when(NRS.Date.Death > z_max_date_death  ~ z_max_date_death,
#                                    TRUE ~ NRS.Date.Death),
#         date_icu_death = case_when(date_icu_death > z_max_date_icu ~ z_max_date_icu,
#                                    TRUE ~ date_icu_death ) )
EAVE_cohort <- EAVE_cohort %>% mutate(death_covid = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ 0,
                                                       TRUE ~ death_covid),
                            icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ 0,
                                                       TRUE ~ icu_death ) )


rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))
rg <- rg %>% dplyr::select(EAVE_LINKNO:EAVE_CHRONIC_LIVER_DIS, EAVE_CHRONIC_LIVER_DIS:EAVE_DIABETES, 
                           EAVE_HYPERTENSION, n_risk_gps)
z <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE_BP_Smoke.rds"))
z <- filter(z, !duplicated(EAVE_LINKNO))
z <- z %>% dplyr::select(EAVE_LINKNO, EAVE_Smoking_Status_Worst, EAVE_BP) %>% 
  dplyr::rename(EAVE_Smoke = EAVE_Smoking_Status_Worst)

rg <- rg %>% left_join(z, by="EAVE_LINKNO")

#read in the Previous Tests data
z  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Tests.RDS")) %>% 
  dplyr::select(EAVE_LINKNO, n_tests)
rg <- rg %>% left_join(z, by="EAVE_LINKNO")
rg <- rg %>% mutate(n_tests = if_else(is.na(n_tests), 0L, n_tests))


#still to do this bit 
#z <- read_csv(paste0(Location,"EAVE/GPanalysis/data/map_files/","Datazone2011Lookup.csv"))
#df <- df %>% left_join(dplyr::select(z,DataZone, Council, HB), by="DataZone")

#saveRDS(df, paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_SGene.rds"))
#df <- df %>% dplyr::rename("EAVE_Smoke" = "EAVE_Smoking_Status_Worst")
################ end of bit still to do

#read in the GP vaccination data
z  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/cleaned_data/C19vaccine.rds"))

print(unique(z$type))
print(unique(z$stage))

Vaccinations <- z %>% mutate(Date = as.Date(occurrence_time)) %>% 
  mutate(vacc_type = case_when(grepl("COURAGEOUS", type) ~ "PB",
                               grepl("TALENT", type) ~ "AZ",
                               type == "39114911000001105" ~ "AZ",
                               type == "39115611000001103" ~ "PB",
#                              type == "39115611000001105" ~ "PB",
                               TRUE ~ "UNK"), 
         dose_number = if_else(stage %in% c(0,3), 1L, stage))

v1 <- filter(Vaccinations, dose_number==1) %>% 
  dplyr::select(EAVE_LINKNO, Date, vacc_type, dose_number) %>% 
  arrange(EAVE_LINKNO, Date) %>% 
  filter(!duplicated(EAVE_LINKNO))
v2 <- filter(Vaccinations, dose_number==2) %>% 
  dplyr::select(EAVE_LINKNO, Date, vacc_type, dose_number) %>% 
  arrange(EAVE_LINKNO, Date) %>% 
  filter(!duplicated(EAVE_LINKNO))

Vaccinations <- left_join(v1,v2, by="EAVE_LINKNO") %>% 
  mutate(date_vacc_1 = as.Date(Date.x), 
         date_vacc_2 = as.Date(Date.y) ) %>% 
  dplyr::rename(vacc_type=vacc_type.x,
                vacc_type_2=vacc_type.y) %>% 
  dplyr::select(-dose_number.x, -dose_number.y, -Date.x, -Date.y)
rm(z,v1,v2)

print(table(Vaccinations$vacc_type, Vaccinations$vacc_type_2, exclude=NULL))
#omit inconsistent records
Vaccinations <- Vaccinations %>% filter(vacc_type %in% c("AZ","PB")) %>% 
  filter(vacc_type_2 %in% c("AZ","PB") | is.na(vacc_type_2)) %>% 
  filter( !(!is.na(vacc_type_2) & (vacc_type_2 != vacc_type)))

#read in the PHS vaccination data -- be careful about the name
#z  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Vaccination.RDS"))
#table(z$EAVE_LINKNO %in% Vaccinations$EAVE_LINKNO)
#table(Vaccinations$EAVE_LINKNO %in% z$EAVE_LINKNO)
#Vaccinations  <- filter(Vaccinations, !(EAVE_LINKNO %in% z$EAVE_LINKNO))
#z1 <- z %>% dplyr::rename(vacc_type = vacc_type_desc, vacc_type_2 = vacc_type_desc_2) %>% 
#  dplyr::select(EAVE_LINKNO, vacc_type, vacc_type_2, date_vacc_1, date_vacc_2 ) %>% as.data.frame()


#z2 <- bind_rows(z1,Vaccinations)
#table(as.numeric(z2$date_vacc_2-z2$date_vacc_1))

#Vaccinations <- z2
rm(z,z1,z2)

#read in covid hositalisations since Dec 01
#derived from linking ecoss to rapid - first admission following a positive test
#this file may have people already in hospital excluded
#we should be able to use the endpoints file for this.
#the read in data are admission following a positive test
#data derived in Chris/Respiratory/Coronavirus/Vaccine_Effect
#covid_hospitalisations  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/covid_hospitalisations.RDS"))
#covid_hospitalisations  <- covid_hospitalisations  %>%  dplyr::select(EAVE_LINKNO, admission_date)

#use the EAVE hospitalisations
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, hosp_covid, date_hosp_covid, NRS.Date.Death) %>% 
  filter(hosp_covid==1) %>% 
  filter(date_hosp_covid > a_begin) %>% 
  dplyr::rename(admission_date = date_hosp_covid) %>% 
  mutate(admission_date = if_else(admissions_date <= NRS.Date.Death, admissions_date, NRS.Date.Death))
  dplyr::select(-hosp_covid, -NRS.Date.Death)
covid_hospitalisations <- z

#use the EAVE severe cases
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, icu_death, date_icu_death) %>% 
  filter(icu_death==1) %>% 
  filter(date_icu_death > a_begin) %>% 
  dplyr::rename(admission_date = date_icu_death) %>% 
  dplyr::select(-icu_death)
covid_icu_death <- z

#use the EAVE death cases
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, death_covid, NRS.Date.Death) %>% 
  filter(death_covid==1) %>% 
  filter(NRS.Date.Death > a_begin) %>% 
  dplyr::rename(admission_date = NRS.Date.Death) %>% 
  dplyr::select(-death_covid)
covid_death <- z

#use the EAVE death cases
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, death_covid, NRS.Date.Death) %>% 
  filter(!is.na(NRS.Date.Death)) %>% 
  filter(NRS.Date.Death > a_begin) %>% 
  dplyr::rename(admission_date = NRS.Date.Death) %>% 
  dplyr::select(-death_covid)
any_death <- z

Positive_Tests <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Positive_Tests.RDS"))

#cohort + risk groups
df_cohort <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO:ur6_2016_name, age_gp, eave_weight) %>% 
  left_join(rg, by="EAVE_LINKNO")

z <- Positive_Tests %>%  mutate(days = as.numeric(specimen_date - a_begin)) %>% 
  mutate(test_before_dec8 = cut(days, breaks = c((min(days)-1), -28, -21, -14, -7, 0, max(days)),
                       labels=c("1+m", "4w","3w","2w","0-6d","post-vacc")))
df_cohort <- df_cohort %>% left_join(dplyr::select(z, EAVE_LINKNO, test_before_dec8), by="EAVE_LINKNO")
df_cohort <- df_cohort %>% mutate(test_before_dec8 = as.character(test_before_dec8)) %>% 
  mutate(test_before_dec8 = if_else(is.na(test_before_dec8), "no pos test",test_before_dec8) )

z <- readRDS(paste0(project_path,"/output/temp/Qcovid.rds"))
z <- z %>% dplyr::select(-(Sex:age_gp), -Q_BMI)
z <- filter(z, !duplicated(EAVE_LINKNO))
z1 <- df_cohort %>% dplyr::select(-(EAVE_ASTHMA:EAVE_HYPERTENSION), -EAVE_Smoke, -EAVE_BP, -n_risk_gps, -eave_weight) %>% 
  left_join(z, by="EAVE_LINKNO")
z1 <- z1 %>% mutate(n_tests = if_else(is.na(n_tests),0L,n_tests) )

df_cohort <- filter(z1, !is.na(eave_weight)) #omit any who - need to fix -  do not match

z <- df_cohort %>% left_join(dplyr::select(Vaccinations, EAVE_LINKNO, date_vacc_1)) %>% 
  mutate(vacc=if_else(is.na(date_vacc_1),0,1))
z_sub <- slice_sample(z, n=round(nrow(z)*0.25))
library(mgcv)
z1 <- glm(vacc ~ age_gp*Sex + simd2020_sc_quintile + n_tests + n_risk_gps,
                data=z_sub, family=binomial)
z_ps <- predict(z1, newdata=z, type="response")
 z <- z %>% mutate(prop_score = z_ps) %>% 
   mutate(inv_psw = if_else(vacc==1, 1/prop_score, (1-vacc)/(1-prop_score))) %>% 
   mutate(inv_psw = if_else(inv_psw > quantile(inv_psw, 0.95), quantile(inv_psw, 0.95), inv_psw)) %>% 
   mutate(inv_psw = inv_psw/mean(inv_psw))

 df_cohort <- z %>% dplyr::select(-date_vacc_1, -vacc)
 
 #adjustment variables
 variables_hosp <- c("age_gp" , "EAVE_BP","Q_DIAG_DIABETES_2"   , "Q_DIAG_COPD"  ,       
                     "Q_DIAG_CKD_LEVEL","Q_DIAG_DEMENTIA","Q_DIAG_STROKE","Q_LEARN_CAT"   ,      
                     "Q_DIAG_FRACTURE","Q_DIAG_NEURO","Q_DIAG_CCF","Q_DIAG_ASTHMA"    ,   
                     "Q_DIAG_EPILEPSY","Q_DIAG_BLOOD_CANCER","Q_DIAG_VTE","Q_DIAG_CIRRHOSIS" ,   
                     "Q_DIAG_RA_SLE","Q_DIAG_PVD","Q_DIAG_AF","Q_DIAG_PULM_RARE" ,   
                     "Q_DIAG_PARKINSONS","Q_DIAG_DIABETES_1","Q_DIAG_PULM_HYPER")
 