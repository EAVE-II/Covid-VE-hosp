## 01_Population_Hospitalisations.R
# run 01a_Vaccination_Input.R to get the data sets


library(plyr)
library(tidyverse)
library(survival)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

project_path <- paste0(Location,"EAVE/GPanalysis/progs/CR/Vaccine")

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-02-24.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))

table(EAVE_cohort$death_covid, is.na(EAVE_cohort$NRS.Date.Death), exclude=NULL)
table(EAVE_cohort$icu_death, is.na(EAVE_cohort$date_icu_death), exclude=NULL)
table(EAVE_cohort$hosp_covid, is.na(EAVE_cohort$date_hosp_covid), exclude=NULL)


a_begin <- as.Date("2020-12-08")
#remove all who have died before the beginning
#EAVE_cohort <- filter(EAVE_cohort, is.na(NRS.Date.Death) | (!is.na(NRS.Date.Death) & NRS.Date.Death > a_begin))
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



df <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
         group_by(date_hosp_covid, age_gp) %>% 
         dplyr::summarise(R=sum(hosp_covid) ) %>% 
  filter(!is.na(date_hosp_covid))
z <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
  group_by(age_gp) %>% 
  dplyr::summarise(Pop=round(sum(eave_weight)) )
df <- df %>% left_join(z, by="age_gp") 
df <- df %>% mutate(rate=R/Pop*100000) %>% as.data.frame() %>% 
  filter(date_hosp_covid >= as.Date("2020-09-01")) %>% 
  mutate(days = as.numeric(date_hosp_covid - min(date_hosp_covid)))

z <- df %>% group_by(date_hosp_covid) %>% dplyr::summarise(N=sum(R))
df <- df %>% left_join(z, by="date_hosp_covid")
df <- df %>%  mutate(perc = R/N*100) %>% 
  filter(date_hosp_covid <= as.Date("2021-02-18"))

z <- mgcv::gam(R ~ offset(log(Pop)) + s(days, by=age_gp) +age_gp, data=df, family=quasipoisson)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(rate_exp = exp(z_pred$fit)/Pop*100000, 
                    rate_lcl = exp(z_pred$fit - 1.96*z_pred$se.fit) /Pop*100000,
                    rate_ucl = exp(z_pred$fit + 1.96*z_pred$se.fit) /Pop*100000)

z <- mgcv::gam(cbind(R, N-R) ~  + s(days, by=age_gp) +age_gp, data=df, family=binomial)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(perc_exp = 100*exp(z_pred$fit)/(1+exp(z_pred$fit)), 
                    perc_lcl = 100*exp(z_pred$fit - 1.96*z_pred$se.fit)/(1+exp(z_pred$fit - 1.96*z_pred$se.fit)),
                    perc_ucl = 100*exp(z_pred$fit + 1.96*z_pred$se.fit)/(1+exp(z_pred$fit + 1.96*z_pred$se.fit)))



df %>% ggplot(aes(x=date_hosp_covid, y=rate, colour=age_gp))+geom_point() + geom_line(aes(y=rate_exp), size=1)+
  geom_line(aes(y=rate_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=rate_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of hospital admission (2020-21)", y="Rate per 100,000", colour="Age Group")


df %>% ggplot(aes(x=date_hosp_covid, y=perc, colour=age_gp))+geom_point() + geom_line(aes(y=perc_exp), size=1)+
  geom_line(aes(y=perc_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=perc_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of hospital admission (2020-21)", y="Percentage in each age group", colour="Age Group")

df %>% ggplot(aes(x=date_hosp_covid, y=R, colour=age_gp))+geom_point() + geom_line(aes(y=Pop*rate_exp/100000), size=1)+
#  geom_line(aes(y=Pop*rate_lcl/100000), size=0.8, linetype="dashed") +
#  geom_line(aes(y=Pop*rate_ucl/100000), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of hospital admission (2020-21)", y="Number admitted", colour="Age Group")

##########################
#deaths

df <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
  group_by(NRS.Date.Death, age_gp) %>% 
  dplyr::summarise(R=sum(death_covid) ) %>% 
  filter(!is.na(NRS.Date.Death))
z <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
  group_by(age_gp) %>% 
  dplyr::summarise(Pop=round(sum(eave_weight)) )
df <- df %>% left_join(z, by="age_gp") 
df <- df %>% mutate(rate=R/Pop*100000) %>% as.data.frame() %>% 
  filter(NRS.Date.Death >= as.Date("2020-09-01")) %>% 
  mutate(days = as.numeric(NRS.Date.Death - min(NRS.Date.Death)))

z <- df %>% group_by(NRS.Date.Death) %>% dplyr::summarise(N=sum(R))
df <- df %>% left_join(z, by="NRS.Date.Death")
df <- df %>%  mutate(perc = R/N*100) %>% 
  filter(NRS.Date.Death <= as.Date("2021-02-17"))

z <- mgcv::gam(R ~ offset(log(Pop)) + s(days, by=age_gp) +age_gp, data=df, family=quasipoisson)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(rate_exp = exp(z_pred$fit)/Pop*100000, 
                    rate_lcl = exp(z_pred$fit - 1.96*z_pred$se.fit) /Pop*100000,
                    rate_ucl = exp(z_pred$fit + 1.96*z_pred$se.fit) /Pop*100000)

z <- mgcv::gam(cbind(R, N-R) ~  + s(days, by=age_gp) +age_gp, data=df, family=binomial)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(perc_exp = 100*exp(z_pred$fit)/(1+exp(z_pred$fit)), 
                    perc_lcl = 100*exp(z_pred$fit - 1.96*z_pred$se.fit)/(1+exp(z_pred$fit - 1.96*z_pred$se.fit)),
                    perc_ucl = 100*exp(z_pred$fit + 1.96*z_pred$se.fit)/(1+exp(z_pred$fit + 1.96*z_pred$se.fit)))



df %>% ggplot(aes(x=NRS.Date.Death, y=rate, colour=age_gp))+geom_point() + geom_line(aes(y=rate_exp), size=1)+
  geom_line(aes(y=rate_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=rate_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of Death (2020-21)", y="Rate per 100,000", colour="Age Group")


df %>% ggplot(aes(x=NRS.Date.Death, y=perc, colour=age_gp))+geom_point() + geom_line(aes(y=perc_exp), size=1)+
  geom_line(aes(y=perc_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=perc_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of Death (2020-21)", y="Percentage in each age group", colour="Age Group")

df %>% ggplot(aes(x=NRS.Date.Death, y=R, colour=age_gp))+geom_point() + geom_line(aes(y=Pop*rate_exp/100000), size=1)+
  #  geom_line(aes(y=Pop*rate_lcl/100000), size=0.8, linetype="dashed") +
  #  geom_line(aes(y=Pop*rate_ucl/100000), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of Death (2020-21)", y="Number deaths", colour="Age Group") +
  scale_y_log10()

########################################
#testing equality of slopes
z_z <- df %>% ungroup() %>% filter(NRS.Date.Death >= as.Date("2020-12-26") + 21)
z_z <- df %>% ungroup() %>% filter(NRS.Date.Death >= as.Date("2020-11-07") & NRS.Date.Death <= as.Date("2020-12-11") )
z_z <- z_z %>%  mutate(days = as.numeric(NRS.Date.Death - min(NRS.Date.Death)) )
z_z$age_gp <- relevel(z_z$age_gp, ref="80+")

z <- glm(R ~ age_gp + days + age_gp:days, data= z_z, family=quasipoisson )
drop1(z,test="F")
z <- glm(R ~ age_gp + age_gp:days, data= z_z, family=quasipoisson )
summary(z)
drop1(z,test="Chisq")


#####################################################
#cases

df <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
  group_by(SpecimenDate, age_gp) %>% 
  dplyr::summarise(R=sum(result) ) %>% 
  filter(!is.na(SpecimenDate))
z <- EAVE_cohort %>% mutate(age_gp = cut(ageYear, breaks =c(-1,64, 79, 110), labels=c("18-64","65-79","80+")) ) %>% 
  group_by(age_gp) %>% 
  dplyr::summarise(Pop=round(sum(eave_weight)) )
df <- df %>% left_join(z, by="age_gp") 
df <- df %>% mutate(rate=R/Pop*100000) %>% as.data.frame() %>% 
  filter(SpecimenDate >= as.Date("2020-09-01")) %>% 
  mutate(days = as.numeric(SpecimenDate - min(SpecimenDate)))

z <- df %>% group_by(SpecimenDate) %>% dplyr::summarise(N=sum(R))
df <- df %>% left_join(z, by="SpecimenDate")
df <- df %>%  mutate(perc = R/N*100) %>% 
  filter(SpecimenDate <= as.Date("2021-02-18"))

z <- mgcv::gam(R ~ offset(log(Pop)) + s(days, by=age_gp) +age_gp, data=df, family=quasipoisson)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(rate_exp = exp(z_pred$fit)/Pop*100000, 
                    rate_lcl = exp(z_pred$fit - 1.96*z_pred$se.fit) /Pop*100000,
                    rate_ucl = exp(z_pred$fit + 1.96*z_pred$se.fit) /Pop*100000)

z <- mgcv::gam(cbind(R, N-R) ~  + s(days, by=age_gp) +age_gp, data=df, family=binomial)
z_pred <- predict.gam(z, se.fit=TRUE) %>% as.data.frame()
df <- df %>% mutate(perc_exp = 100*exp(z_pred$fit)/(1+exp(z_pred$fit)), 
                    perc_lcl = 100*exp(z_pred$fit - 1.96*z_pred$se.fit)/(1+exp(z_pred$fit - 1.96*z_pred$se.fit)),
                    perc_ucl = 100*exp(z_pred$fit + 1.96*z_pred$se.fit)/(1+exp(z_pred$fit + 1.96*z_pred$se.fit)))



df %>% ggplot(aes(x=SpecimenDate, y=rate, colour=age_gp))+geom_point() + geom_line(aes(y=rate_exp), size=1)+
  geom_line(aes(y=rate_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=rate_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date Specimen (2020-21)", y="Rate per 100,000", colour="Age Group")


df %>% ggplot(aes(x=SpecimenDate, y=perc, colour=age_gp))+geom_point() + geom_line(aes(y=perc_exp), size=1)+
  geom_line(aes(y=perc_lcl), size=0.8, linetype="dashed") +geom_line(aes(y=perc_ucl), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of Specimen (2020-21)", y="Percentage in each age group", colour="Age Group")

df %>% ggplot(aes(x=SpecimenDate, y=R, colour=age_gp))+geom_point() + geom_line(aes(y=Pop*rate_exp/100000), size=1)+
  #  geom_line(aes(y=Pop*rate_lcl/100000), size=0.8, linetype="dashed") +
  #  geom_line(aes(y=Pop*rate_ucl/100000), size=0.8, linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-12-08"), size=0.7, linetype="dotted") + 
  geom_vline(xintercept=as.Date("2020-12-26"), col="blue", size=0.7, linetype="dotted") +
  geom_vline(xintercept=as.Date("2021-01-5"), col="blue", size=0.7, linetype="dotted") +
  labs(x="Date of Specimen (2020-21)", y="Number Positive", colour="Age Group")

