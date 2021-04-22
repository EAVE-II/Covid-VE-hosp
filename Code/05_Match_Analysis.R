##########################################################
# Name of file: 05_Match_Analysis.R
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
#                         run 05_Match.R to set up the mathed at vaccination  study
#                         matching on Age, Sex plus
#                         this script fits the models
# Approximate run time: Unknown
##########################################################


library(survival)
z_main<- ""
z <- survfit(Surv(time_to_hosp, event) ~ vacc  , data=df_cc, subset=ageYear >= 80 & ageYear <= 110 & vacc_type=="AZ")
plot(z, fun="event", col=c(1,2), conf.int=T)
title(main=z_main, xlab="days from vaccination",ylab="cumulative risk", sub="age 80+")
legend("topleft",legend=levels(df_cc$vacc), lty=1, col=c(1,2), cex=0.8)

z <- survfit(Surv(time_to_hosp, event) ~ vacc  , data=df_cc, subset=ageYear >= 65 & ageYear <= 79 & vacc_type=="PB")
plot(z, fun="event", col=c(1,2), conf.int=T)
title(main=z_main, xlab="days from vaccination",ylab="cumulative risk", sub="age 65-79")
legend("topleft",legend=levels(df_cc$vacc), lty=1, col=c(1,2), cex=0.8)

z <- survfit(Surv(time_to_hosp, event) ~ vacc  , data=df_cc, subset=ageYear >= 18 & ageYear <= 64 )
plot(z, fun="event", col=c(1,2), conf.int=T)
title(main="Hospitalisation", xlab="days from vaccination",ylab="cumulative risk", sub="age 18-64")
legend("topleft",legend=levels(df_cc$vacc), lty=1, col=c(1,2), cex=0.8)


table(df_cc$vacc, df_cc$event)
table(df_cc$age_gp, df_cc$event, df_cc$vacc_type, df_cc$vacc)

table(df_cc$simd2020_sc_quintile, df_cc$vacc)
table(df_cc$Q_DIAG_DEMENTIA, df_cc$vacc)
table(df_cc$EAVE_Smoke, df_cc$vacc)


#z.yr <- tcut(rep(0,nrow(z_cc)), c(-1,seq(7,max(z_cc$time_to_hosp),by=7) ))
#z.yr <- tcut(rep(0,nrow(df_cc)), c(-1,seq(0,max(z_cc$time_to_hosp),by=1) ))
z.yr <- tcut(rep(0,nrow(df_cc)), c(-1,21,42, max(df_cc$time_to_hosp) ) )
z.yr <- tcut(rep(0,nrow(df_cc)), c(-1,14,28,42, max(df_cc$time_to_hosp) ) )
#aggregate for overall
z.agg <- pyears(Surv(time_to_hosp,event) ~ vacc + z.yr,
                data=df_cc , scale=1, data.frame=TRUE)

df_res <- z.agg$data
names(df_res) <- c("Vacc", "Period", "Days.At.Risk","Count","Events")
df_res <- df_res %>% mutate(Rate = Events/Days.At.Risk*365.25) %>% 
  group_by(Period) %>% 
  mutate(RR = Rate/first(Rate)) %>% ungroup()
df_res

#aggregate for overall
z.agg <- pyears(Surv(time_to_hosp,event) ~ vacc + z.yr + age_gp + Sex + simd2020_sc_quintile + vacc_type + n_risk_gps +n_tests_gp,
                data=df_cc , scale=1, data.frame=TRUE)

z_pois <- z.agg$data
#pois$age_gp <- relevel(z_pois$age_gp, ref="60-64")



z_pois$day <- as.numeric(z_pois$z.yr)-1
z_pois$day2 <- z_pois$day*(z_pois$vacc=="vacc")
z_pois <- filter(z_pois, day < 64)
z_pois$vacc <- factor(z_pois$vacc)

z_nd <- z_pois

z_m <- glm(event ~ offset(log(pyears)) + z.yr +z.yr:vacc + age_gp + Sex + simd2020_sc_quintile + n_risk_gps +n_tests_gp, 
           family=poisson, data=z_pois, subset=as.numeric(age_gp) >= 17)
z_m <- mgcv::gam(event ~ offset(log(pyears)) + s(day, by=vacc) + vacc, family=poisson, data=z_pois)
z_m <- mgcv::gam(event ~ offset(log(pyears)) + s(day) + as.factor(vacc), family=poisson, data=z_pois)
z_m <- mgcv::gam(event ~ offset(log(pyears)) + s(day) + as.factor(vacc) + s(day2), family=poisson, data=z_pois)

summary(z_m)

z_p <- predict(z_m, newdata=z_nd, type="iterms")

z_nd$fit <- predict(z_m, newdata=z_nd, type="response")
z_nd$prate <- z_nd$fit/z_nd$pyears
z_nd$id <- 1:nrow(z_nd)
z <- pivot_wider(z_nd, id_cols=day, names_from = vacc, values_from = prate)
z$ratio <- z$vacc/z$uv
plot(z$day, z$ratio)
abline(h=1)

z_m <- mgcv::gam(event ~ offset(log(pyears)) + s(day, by=vacc) + vacc, family=poisson, data=z_pois)
z_p <- mgcv::predict.gam(z_m, newdata=z_nd, type="lpmatrix")
#columns
c_uv <- grepl(":vaccuv", colnames(z_p)) #smoothcolumns for unvacc
c_vacc <- grepl(":vaccvacc", colnames(z_p))  #smooth columns for vacc
#rows
r_uv <- with(z_nd, vacc=="uv")
r_vacc <- with(z_nd, vacc=="vacc")
#difference
X <- z_p[r_vacc,] - z_p[r_uv,]
#zero other columns
X[,!(c_uv | c_vacc)] <- 0
#set vaccine column to 1 to get the effect
X[, "vaccvacc"] <- 1
#offset difference
offset_diff <- log(z_nd[r_uv,"pyears"]) - log(z_nd[r_vacc,"pyears"]) 

difference <- X %*% coef(z_m) + offset_diff
se_difference <- sqrt(rowSums((X %*% vcov(z_m, unconditional=TRUE)) * X))
z_lcl <- difference - 1.96*se_difference
z_ucl <- difference + 1.96*se_difference

z$rr <- exp(difference)
z$rr_lcl <- exp(z_lcl)
z$rr_ucl <- exp(z_ucl)
#truncate for plot
z$rr_ucl[z$rr_ucl>=3] <- 3
z %>% ggplot(aes(x=day))  + geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), fill="grey70") +geom_line(aes(y=rr)) +
  geom_hline(yintercept=1) +lims(y=c(0,3))

z_m <- glm(event ~ offset(log(pyears)) + z.yr + z.yr:vacc, family=poisson, data=z_pois)
z_ci <- exp(confint.default(z_m))
z_est <- exp(coef(z_m))
z_glm <- as.data.frame(cbind(z_est,z_ci)[64:126,])
z_glm$day <- 1:63
names(z_glm) <- c("rr","rr_lcl","rr_ucl","day")
z_glm <- filter(z_glm, is.finite(rr_ucl))
z_glm %>% ggplot(aes(x=day))  + geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), fill="grey70") +geom_line(aes(y=rr)) +
  geom_hline(yintercept=1) +lims(y=c(0,5))

#put the df_cc into long format 
z <- df_cc %>% dplyr::select(EAVE_LINKNO, EAVE_LINKNO_vacc, vacc, time_to_hosp, event) %>% 
  mutate(tstart=0, day1=14, day2=28, day3=42) 
z1 <- filter(z, time_to_hosp==0)
z <- z %>% filter(!(EAVE_LINKNO_vacc %in% z1$EAVE_LINKNO_vacc))
z <- z %>%   mutate(id=1:nrow(z))

z1 <- tmerge(z,z,id=id, endpt = event(time_to_hosp, event))
z1 <- tmerge(z1,z, id=id, day1=tdc(day1))
z1 <- tmerge(z1,z, id=id, day2=tdc(day2))
z1 <- tmerge(z1,z, id=id, day3=tdc(day3))

z <- z1 %>% 
  mutate(Period = factor(day1+day2+day3, levels=0:3, labels =c("0:14","15:28","29:42","43+"))) %>% 
  dplyr::select(-event, -time_to_hosp, -(day1:day3), -id) %>% 
  dplyr::rename(event=endpt)

z <- z %>% left_join(dplyr::select(df_cc, -(EAVE_LINKNO_vacc:time_to_hosp)), by="EAVE_LINKNO")  
z <- z %>% mutate(pyears=tstop-tstart)
df_cc_long <- z 

variables_covid_death <- c("Q_DIAG_AF","Q_DIAG_BLOOD_CANCER", "Q_DIAG_CCF" ,"Q_DIAG_CHD" ,"Q_DIAG_CIRRHOSIS",   
    "Q_DIAG_CONGEN_HD",   "Q_DIAG_COPD",         "Q_DIAG_DEMENTIA",     "Q_DIAG_DIABETES_1",   "Q_DIAG_DIABETES_2",  
    "Q_DIAG_PARKINSONS",   "Q_DIAG_PULM_HYPER",   "Q_DIAG_PULM_RARE",    "Q_DIAG_PVD",          "Q_DIAG_RA_SLE",      
     "Q_DIAG_RESP_CANCER",  "Q_DIAG_SEV_MENT_ILL", "Q_DIAG_VTE")
#aggregate
z.fmla <- as.formula(paste("event ~ offset(log(pyears)) + Period + Period:vacc + age_gp + Sex + simd2020_sc_quintile + n_risk_gps +n_tests_gp +",
                           paste(variables_covid_death, collapse= "+")))

df_cc_long$age_gp <- relevel(df_cc_long$age_gp, ref="60-64")

#z_pois$day <- as.numeric(z_pois$z.yr)-1
z_m <- glm(z.fmla,family=poisson, data=df_cc_long, subset=as.numeric(age_gp) >= 13 & as.numeric(age_gp) <= 16)

#z_m <- mgcv::gam(event ~ offset(log(pyears)) + s(day, by=as.factor(vacc)) + as.factor(vacc), family=poisson, data=z_pois)
summary(z_m)
exp(cbind(z_m$coefficients, confint.default(z_m) ) )


