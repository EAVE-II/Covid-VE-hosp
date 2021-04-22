
z_df$n_tests_gp <- cut(z_df$n_tests, breaks = c(-1,0,1,2,3,9,100), labels=c("0","1","2","3","4-9","10+"))
z.agg <- pyears(Surv(start,stop,event) ~ vacc_status + period + age_gp + simd2020_sc_quintile +
                  Sex + n_risk_gps +n_tests_gp, data=z_df , scale=1, data.frame=TRUE)

z_pois <- z.agg$data

z_m <- glm(event ~ offset(log(pyears)) + vacc_status + period + age_gp + simd2020_sc_quintile+
             Sex + n_risk_gps + n_tests_gp, family=poisson, data=z_pois)
summary(z_m)

