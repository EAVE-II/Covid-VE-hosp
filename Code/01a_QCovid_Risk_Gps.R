

rg <- readRDS("/conf/EAVE/GPanalysis/data/cleaned_data/Qcovid_update_Jan21.rds")

rg <- readRDS( "/conf/EAVE/GPanalysis/data/combined_qcovid_demographics.rds")
rg$Q_BMI[is.na(rg$Q_BMI)] <- 0 
rg$Q_LEARN_CAT[is.na(rg$Q_LEARN_CAT)] <- 0 
rg$Q_HOME_CAT[is.na(rg$Q_HOME_CAT)] <- 0


rg <- filter(rg, !duplicated(EAVE_LINKNO))

rg <- readRDS("/conf/EAVE/GPanalysis/progs/EM/Vaccines/vaccinput.rds")

rg <- rg %>% dplyr::select(EAVE_LINKNO:ur6_2016_name, age_gp, Q_DIAG_AF:EAVE_BP, eave_weight) %>% 
  dplyr::select(-Q_ETHNICITY)
rg <- rg %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~replace(., is.na(.), 0))
rg <- rg %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~as.numeric(.))

z1 <- rg %>% dplyr::select(Q_DIAG_AF:Q_DIAG_VTE, Q_HOME_CAT:Q_DIAG_CKD_LEVEL)
z1 <- z1 %>%  mutate_at(vars(Q_HOME_CAT:Q_DIAG_CKD_LEVEL), ~ifelse(. >1,1,.))
z1_total <- apply(z1,1,sum)
z1_n_risk_gps <- cut(z1_total, breaks =c(-1,0,1,2,3,4,15), labels=c("0","1","2","3","4","5+"))

rg$n_risk_gps <- z1_n_risk_gps

rg <- rg %>% mutate(EAVE_BP = if_else(is.na(EAVE_BP), "Unknown", as.character(EAVE_BP)),
                   EAVE_Smoke = if_else(is.na(EAVE_Smoke), "Unknown", as.character(EAVE_Smoke)) ) %>% 
  mutate(Q_BMI = na_if(Q_BMI,0)) %>% 
  mutate(Q_BMI = if_else(Q_BMI>50,NA_real_,Q_BMI),
         Q_BMI = if_else(Q_BMI<12,NA_real_,Q_BMI) )
###############################

#Calculate the imputed values for bmi

z_miss_bmi <- filter(rg, is.na(Q_BMI)) %>% 
  dplyr::select(ageYear, Sex, Q_DIAG_AF:Q_DIAG_VTE, Q_HOME_CAT, Q_LEARN_CAT, Q_DIAG_CKD_LEVEL, EAVE_LINKNO) %>% 
  mutate(Sex=if_else(Sex=="M",1,0)) %>% 
  mutate(town=0) %>% 
  relocate(town,.after=Sex)

pred_bmi_coefs <- c(25.454589211,  0.034819271, -0.200758681,  0.078336103,  0.349318439,
                    0.935957507, -0.676673763, 0.206517652, -0.831372977,  0.215348398,  0.205422595,
                    -0.037882696, -1.186753441, -2.815319705, 0.371056996,  3.423302834,  0.082725341,
                    -0.994328752, -0.432617520, -1.314852087, -0.667638044, -1.247299417, -1.348456939,
                    -0.005665688, -1.775427465,  0.886037579, -0.595863315, -0.521845996,
                    1.039846838, -1.171865402,  0.743896428, -0.100554399, -0.375900747, -0.386150319)
names(pred_bmi_coefs) <- 
  c("(Intercept)",          "Age",                  "SexM",                 "town",                
    "Q_DIAG_AF",            "Q_DIAG_ASTHMA",        "Q_DIAG_BLOOD_CANCER",  "Q_DIAG_CCF"  ,        
    "Q_DIAG_CEREBRALPALSY", "Q_DIAG_CHD"   ,        "Q_DIAG_CIRRHOSIS",     "Q_DIAG_CONGEN_HD" ,   
    "Q_DIAG_COPD",          "Q_DIAG_DEMENTIA",      "Q_DIAG_DIABETES_1",    "Q_DIAG_DIABETES_2",   
    "Q_DIAG_EPILEPSY",      "Q_DIAG_FRACTURE",      "Q_DIAG_NEURO",         "Q_DIAG_PARKINSONS",   
    "Q_DIAG_PULM_HYPER",    "Q_DIAG_PULM_RARE",     "Q_DIAG_PVD" ,          "Q_DIAG_RA_SLE",       
    "Q_DIAG_RESP_CANCER",   "Q_DIAG_SEV_MENT_ILL",  "Q_DIAG_SICKLE_CELL",   "Q_DIAG_STROKE",       
    "Q_DIAG_VTE" ,          "Q_HOME_CAT",           "Q_LEARN_CAT" ,         "CKD_levelCKD 3" ,     
    "CKD_levelCKD 4" ,      "CKD_levelCKD 5") 
z_pred <- as.matrix(z_miss_bmi[, 1:30]) %*% pred_bmi_coefs[2:31]
z_pred <- z_pred + pred_bmi_coefs[1] + (z_miss_bmi[,31]== "CKD 3")*pred_bmi_coefs[32] +
  (z_miss_bmi[,31]== "CKD 4")*pred_bmi_coefs[33] +
  (z_miss_bmi[,31]== "CKD 5")*pred_bmi_coefs[34]
z_pred <- as.numeric(z_pred[,1])
z_pred <- data.frame(EAVE_LINKNO = z_miss_bmi$EAVE_LINKNO, bmi_impute=z_pred)

rg <- rg %>% left_join(z_pred, by="EAVE_LINKNO")
rg <- rg %>% mutate(bmi_impute= if_else(is.na(bmi_impute), Q_BMI, bmi_impute))

saveRDS(rg, "/conf/EAVE/GPanalysis/progs/CR/Vaccine/output/temp/Qcovid.rds")

##############################


#get Vaccinations from 01a_Vaccinations_Input.R

df <- left_join(df_cohort, dplyr::select(Vaccinations, EAVE_LINKNO, date_vacc_1, vacc_type, date_vacc_2), by="EAVE_LINKNO")
df <- df %>% mutate(vacc= if_else(is.na(date_vacc_1), 0,1),
                    vacc_type= if_else(is.na(vacc_type), "uv",vacc_type))
df <- df %>% mutate(age_gp2 = cut(ageYear, c(17,64,79,120), labels = c( "18-64","65-79","80+")))
#df <- df %>% mutate(bmi_gp = cut(Q_BMI, c(12, 19, 24,29,34,39,100))) %>% 
#  mutate(bmi_gp = as.character(bmi_gp)) %>% 
#  mutate(bmi_gp = if_else(is.na(bmi_gp), "Unknown",bmi_gp))
population <- df %>% group_by(age_gp2) %>% dplyr::summarise(N=round(sum(eave_weight)))
Population <- list(pop5 = population)
Population$pop3gps <- population
#modify age_gp2 for the vaccination uptake
df <- df %>% mutate(age_gp2 = cut(ageYear+1, c(16,64,79,120), labels = c( "18-64","65-79","80+")))

######
#
z_t <- table(df$test_before_dec8, df$vacc, exclude=NULL)
z_t
prop.table(z_t,1)
z_t <- table(df$test_before_dec8, df$vacc, (df$EAVE_LINKNO %in% covid_hospitalisations$EAVE_LINKNO), exclude=NULL)
z_t

table(is.na(df$date_vacc_2), exclude=NULL)
table(df$vacc, is.na(df$date_vacc_2), exclude=NULL)

##################################
z <- df %>% group_by(age_gp2, vacc_type) %>% dplyr::summarise(N=n()) %>% 
  filter(vacc_type != "uv") %>% 
  mutate(age=substring(age_gp,1,2))
z %>% ggplot(aes(x=age, y=N, fill= vacc_type)) + geom_col() +
  labs(x="Age Group (lower limit)", y="Number Vaccinated",fill="Vaccine")

##Table 1


z_df <- df %>%
#  filter(age_gp2=="80+") %>% 
  dplyr::select(Sex, age_gp2,simd2020_sc_quintile, ur6_2016_name, n_risk_gps, vacc, vacc_type, eave_weight) %>% 
  pivot_longer(cols=Sex:n_risk_gps) 

z <- z_df %>% group_by(name, value) %>% 
  dplyr::summarise(N=round(sum(eave_weight)), Vaccinated = sum(vacc), 
                   AZ = sum(vacc_type=="AZ"), PB = sum(vacc_type=="PB")) %>% 
  mutate(Unvaccinated=N-Vaccinated, Uptake = Vaccinated/N*100) %>% ungroup()
#z <- z %>%  group_by(name) %>% mutate(RR=Uptake/first(Uptake)) %>% ungroup() %>% as.data.frame()
z2 <- z %>% group_by(name) %>% dplyr::summarise(N_U = sum(Unvaccinated), N_V=sum(Vaccinated), 
                                                N_AZ = sum(AZ), N_PB=sum(PB), F=first(Uptake))
z <- z %>% left_join(z2, by="name") %>% 
  mutate(Percent_of_Vaccinated = round(Vaccinated/N_V*100,1), 
         Percent_of_Unvaccinated = round(Unvaccinated/N_U*100,1),
         Percent_of_AZ = round(AZ/N_AZ*100,1),
         Percent_of_PB = round(PB/N_PB*100,1),
         RR=Uptake/F)
z <- z %>% dplyr::select(name, value, Vaccinated, Percent_of_Vaccinated, Unvaccinated, Percent_of_Unvaccinated,
                         Uptake, RR, AZ, Percent_of_AZ, PB, Percent_of_PB) %>% ungroup() %>% as.data.frame()


z_df <- df %>%
  filter(age_gp2=="80+") %>% dplyr::select(Q_DIAG_ASTHMA, Q_DIAG_CKD_LEVEL, Q_DIAG_CIRRHOSIS,
          Q_DIAG_NEURO, Q_DIAG_DIABETES_1, Q_DIAG_DIABETES_2, Q_DIAG_DEMENTIA, 
          Q_DIAG_CHD, Q_DIAG_CCF,           vacc, vacc_type, eave_weight) %>% 
  mutate(Q_DIAG_CKD_LEVEL = if_else(Q_DIAG_CKD_LEVEL >= 3,1,0) ) %>% 
  pivot_longer(cols=Q_DIAG_ASTHMA:Q_DIAG_CCF)


z_df <- df%>%
  filter(age_gp2=="80+")  %>% dplyr::select(EAVE_Smoke, EAVE_BP,           vacc, vacc_type, eave_weight) %>% 
  pivot_longer(cols=-(vacc:eave_weight))


#############################################
# uses df - df_cohort linked to vaccinations

z_df <- df %>% dplyr::select(EAVE_LINKNO, age_gp2, vacc, vacc_type, date_vacc_1, ageYear) %>% 
  filter(!is.na(date_vacc_1)) %>% 
  group_by(date_vacc_1,age_gp2) %>% 
  dplyr::summarise(n_vacc = sum(vacc), n_pb = sum(vacc_type=="PB"), n_az = sum(vacc_type=="AZ")) %>% 
  ungroup() %>% group_by(age_gp2) %>% 
  mutate(cum_vacc=cumsum(n_vacc)) %>%  ungroup()
z <- df %>% group_by(age_gp2) %>% dplyr::summarise(Pop = sum(eave_weight))
z_df <- z_df %>% left_join(z, by="age_gp2") %>% mutate(perc_cum_vacc = cum_vacc/Pop*100) %>%
  filter(date_vacc_1 <= as.Date("2021-02-18"))


z_df_hosp <- df %>% left_join(covid_hospitalisations, by="EAVE_LINKNO") %>% 
  filter(!is.na(admission_date)) %>% 
  group_by(admission_date, age_gp2) %>% 
  dplyr::summarise(n_hosp=n()) %>% 
  filter(admission_date <= as.Date("2021-02-18"))

z_df_covid_death <- df %>% left_join(covid_death, by="EAVE_LINKNO") %>% 
  filter(!is.na(admission_date)) %>% 
  group_by(admission_date, age_gp2) %>% 
  dplyr::summarise(n_hosp=n()) %>% 
  filter(admission_date <= as.Date("2021-02-18"))


z_df %>% ggplot(aes(x=date_vacc_1, y=n_vacc, colour=age_gp2) ) +geom_line()
g1 <- z_df %>% ggplot(aes(x=date_vacc_1, y=perc_cum_vacc, colour=age_gp2) ) +geom_line()+
  labs(x="Date of Vaccination",y="Cumulative Percentage", colour="Age Group")

g2 <- z_df_hosp %>% ggplot(aes(x=admission_date, y=n_hosp, colour=age_gp2)) + geom_point() +
  geom_smooth(span=0.4, se=FALSE, size=0.7) +
  labs(x="Date of Admission",y="Number Admitted", colour="Age Group")

g2 <- z_df_covid_death %>% ggplot(aes(x=admission_date, y=n_hosp, colour=age_gp2)) + geom_point() +
  geom_smooth(span=0.4, se=FALSE, size=0.7) +
  labs(x="Date of death",y="Number Died", colour="Age Group")



library(gridExtra)

grid.arrange(g1,g2, nrow=2, ncol=1)
z_z <- z_df_hosp %>% ungroup() %>% filter(admission_date >= as.Date("2020-12-26") +7)
z_z <- z_z %>%  mutate(days = as.numeric(admission_date - min(admission_date)) ) %>% 
  mutate(days2 = if_else(admission_date <= as.Date("2021-01-24"),0, as.numeric(admission_date - as.Date("2021-01-24")) ))
z_z$age_gp2 <- relevel(z_z$age_gp2, ref="80+")

z_z <- z_df_covid_death %>% ungroup() %>% filter(admission_date >= as.Date("2020-12-26") + 21)
z_z <- z_z %>%  mutate(days = as.numeric(admission_date - min(admission_date)) )

z_z <- z_df_covid_death %>% ungroup() %>% filter(admission_date >= as.Date("2020-12-26") + 21)
z <- glm(n_hosp ~ age_gp2 + days + age_gp2:days, data= z_z, family=quasipoisson )
drop1(z,test="F")
z <- glm(n_hosp ~ -1 + age_gp2 + age_gp2:days, data= z_z, family=quasipoisson )
z <- glm(n_hosp ~ -1 + age_gp2 + age_gp2:days + age_gp2:days2, data= z_z, family=quasipoisson )
summary(z)
drop1(z,test="Chisq")

(exp(cbind(z$coefficients, confint.default(z))) -1)*100


z_z %>% filter(days<=6) %>% group_by(age_gp2) %>% dplyr::summarise(N=sum(n_hosp))
