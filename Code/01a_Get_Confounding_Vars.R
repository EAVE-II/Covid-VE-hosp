# 01a_Get_Confounding_Vars.R

z_event_endpoint <- "hosp_covid" #hosp, death or icu_death
if (z_event_endpoint =="hosp_covid") z_event <- covid_hospitalisations
if (z_event_endpoint =="icu_death") z_event <- covid_icu_death
if (z_event_endpoint =="death_covid") z_event <- covid_death
if (z_event_endpoint =="any_death") z_event <- any_death

df <- df_cohort %>% left_join(z_event, by="EAVE_LINKNO")
df <- df %>% mutate(event = ifelse(is.na(admission_date), 0, 1))
print(table(df$event, exclude=NULL)) #check numebr of events

predictions_vars <- names(df_cohort)
predictions_vars <- predictions_vars[-c(1,3,5,6,9,40,41)]

z_event_ids <- unique(z_event$EAVE_LINKNO)
z_rest <- filter(df, !(EAVE_LINKNO %in% z_event_ids))
z_ids_rest <- unique(z_rest$EAVE_LINKNO)
z_ids_rest <- sample(z_ids_rest, size=nrow(z_event)*20, replace=FALSE)
z_ids <- c(z_event_ids, z_ids_rest)
z_df <- filter(df, EAVE_LINKNO %in% z_ids )
table(z_df$event, exclude=NULL)

z.fmla <- as.formula(paste("event"," ~ ", paste(predictions_vars, collapse= "+")))
z.fit <- glm(formula = z.fmla  , data=z_df, family=binomial, weight=eave_weight)

z.fit <- glm(event ~ age_gp + Sex + simd2020_sc_quintile +bmi_impute   , data=z_df, family=binomial, weight=eave_weight)
z.upper <- as.formula(paste(" ~ ", paste(predictions_vars, collapse= "+")))
z.lower <- as.formula("~ age_gp + Sex + simd2020_sc_quintile")
z <- MASS::stepAIC(z.fit, scope=list(upper=z.upper, lower=z.lower), direction="forward",k=log(nrow(z_df)))

variables_hosp <- names(z$model)
variables_hosp <- variables_hosp[-c(1,29)]
variables_hosp <- c("age_gp" , "EAVE_BP","Q_DIAG_DIABETES_2"   , "Q_DIAG_COPD"  ,       
"Q_DIAG_CKD_LEVEL","Q_DIAG_DEMENTIA","Q_DIAG_STROKE","Q_LEARN_CAT"   ,      
"Q_DIAG_FRACTURE","Q_DIAG_NEURO","Q_DIAG_CCF","Q_DIAG_ASTHMA"    ,   
"Q_DIAG_EPILEPSY","Q_DIAG_BLOOD_CANCER","Q_DIAG_VTE","Q_DIAG_CIRRHOSIS" ,   
"Q_DIAG_RA_SLE","Q_DIAG_PVD","Q_DIAG_AF","Q_DIAG_PULM_RARE" ,   
"Q_DIAG_PARKINSONS","Q_DIAG_DIABETES_1","Q_DIAG_PULM_HYPER")