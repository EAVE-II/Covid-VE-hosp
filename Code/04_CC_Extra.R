z_df <- z_df %>%  mutate(vs = as.character(vacc_status)) %>% 
  mutate(vs = case_when(vs=="uv" ~ "uv",
                        vs %in% c("v1_0:6","v1_7:13") ~ "v1_0:13",
                        TRUE ~ "v1:14+v2")) %>% 
  mutate(vs = factor(vs, levels=c("uv","v1_0:13","v1:14+v2" )))


z_fit <- clogit( event ~ vs + strata(EAVE_LINKNO_case) + hosp + simd2020_sc_quintile,
                 data=z_df )
summary(z_fit)



z_df <- mutate(z_df, bmi_gp = cut(bmi_impute, breaks=c(10, 19, 24,29,34,39, 50)))
z_vars <- names(z_df)[c(19:48,58)] #any_throm
rm(z_out)

for ( i in z_vars){
  #i <- z_vars[1]
  
  z_fit <- try(clogit( event ~ vs + strata(EAVE_LINKNO_case)  +  hosp + simd2020_sc_quintile +
                        as.factor(get(i)) + vs:as.factor(get(i)),
                       data=z_df , method="efron"))
  
  print(i)
  #print(summary(z_fit))
  z <- as.data.frame(summary(z_fit)$conf.int)
  z$var <- rep(i, nrow(z))
  z$names <- row.names(z)
  row.names(z) <- NULL
  
  
  z_out <- if (exists("z_out")) bind_rows(z_out, z) else z
}

z <-  filter(z_out, grepl("vsv1:14\\+v2", names))

z_int <- filter(z_out, grepl("vsv1:14\\+v2:as", names))


z_fit <- clogit( event ~ strata(EAVE_LINKNO_case)  +  hosp + simd2020_sc_quintile +
                        as.factor(Q_HOME_CAT) + as.factor(Q_HOME_CAT):vs,
                     data=z_df , method="efron")
summary(z_fit)

z_fit <- clogit( event ~ strata(EAVE_LINKNO_case)  +  hosp + simd2020_sc_quintile +
                   as.factor(bmi_gp) + as.factor(bmi_gp):vs,
                 data=z_df , method="efron")
summary(z_fit)

z_fit <- clogit( event ~ strata(EAVE_LINKNO_case)  +  hosp + simd2020_sc_quintile +
                   as.factor(bmi_gp) + as.factor(bmi_gp):vs,
                 data=z_df , method="efron")
summary(z_fit)
