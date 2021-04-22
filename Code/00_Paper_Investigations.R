

z_df <- df
z_ids <- unique(z_df$EAVE_LINKNO)
z_ids <- sample(z_ids, size=round(length(z_ids)*0.10))

z_ids_case <- unique(filter(z_df, event==1)$EAVE_LINKNO)
z_ids_cont <- unique(z_df$EAVE_LINKNO)
z_ids_cont <- z_ids_cont[!(z_ids_cont %in% z_ids_case)]
z_ids <- c(z_ids_case, sample(z_ids_cont, size=round(length(z_ids_cont)*0.20)) )
z_df <- filter(z_df, EAVE_LINKNO %in% z_ids)

z_df <- filter(z_df, test_before_dec8 %in% c("no pos test","post-vacc") )
z_df <- mutate(z_df, vacc = if_else(vacc_status=="uv",0,1))
plot(survfit(Surv(start, stop, event) ~ vacc,data=z_df), fun="event", col=1:2, conf.int=TRUE, 
     xlab="Days until Hospitalisation", ylab="Cumulative Proportion", xlim=c(0,35))
legend("topleft",legend=c("Unvaccinated","Vaccinated"), lty=1, col=1:2, cex=0.8)


z <- coxph( Surv(start, stop, event) ~ vacc + pspline(ageYear) + Sex + simd2020_sc_quintile +
              pspline(n_tests, df=2) + n_risk_gps +strata(period)  ,data=z_df)
##no period strata
z_newdata <- z_df %>% dplyr::select(start,stop,event, ageYear, period, vacc, Sex, 
          simd2020_sc_quintile, n_tests, n_risk_gps) %>% 
  filter(row_number() <= 2) %>% 
  mutate(period=levels(period)[1], ageYear=75, vacc=c(0,1), Sex=first(Sex), 
         simd2020_sc_quintile=first(simd2020_sc_quintile), n_tests=0, n_risk_gps=levels(n_risk_gps)[[1]])
z_sf <- survfit(z, newdata=z_newdata, conf.type = "log")
plot(z_sf, fun="event", col=c(1:2), conf.int=TRUE, xlim=c(0,35),
     xlab="Days until Hospitalisation", ylab="Cumulative Proportion")
legend("topleft",legend=c("Unvaccinated","Vaccinated"), lty=1, col=1:2, cex=0.8)

#period strata
z_newdata <- z_df %>% dplyr::select(start,stop,event, ageYear, period, vacc, Sex, 
                                    simd2020_sc_quintile, n_tests, n_risk_gps) %>% 
  filter(row_number() <= 20) %>% 
  mutate(start=rep(seq(0,63,7),2), stop=rep(seq(0,63,7)+1,2), event=0,
    period=rep(levels(period),2), ageYear=75, vacc=c(rep(0,10),rep(1,10)), Sex="M", 
         simd2020_sc_quintile=levels(simd2020_sc_quintile)[1], n_tests=0, n_risk_gps=levels(n_risk_gps)[[1]])


z_sf <- survfit(z, newdata=z_newdata, conf.type = "log")
plot(z_sf, fun="event", col=rep(1:2,c(10,10)), conf.int=TRUE, 
     xlab="Days until Hospitalisation", ylab="Cumulative Proportion")
legend("topleft",legend=c("Unvaccinated","Vaccinated"), lty=1, col=1:2, cex=0.8)


############################

z_coef <- coefficients(z_m)[1:10]
z_coef[1] <- 0
z_vcov <- summary(z_m)$cov.unscaled[1:10,1:10]
z_vcov[1,] <- 0
z_vcov[,1] <- 0


#######################
#comparing confirmed and unconformed hosp covid
z <- filter(EAVE_cohort,hosp_covid==1 & date_hosp_covid > a_begin) %>% 
  left_join(Vaccinations, by="EAVE_LINKNO")

table(z$result)
by(z$ageYear, z$result, summary)
z_t <- table(z$result, z$vacc_type, exclude=NULL)
prop.table(table(z$result, z$vacc_type, exclude=NULL),1)
chisq.test(z_t)
z_t <- table(z$result, z$vacc_type %in% c("AZ","PB"), exclude=NULL)
prop.table(z_t,1)

###############################

