##########################################################
# Name of file: 03b_Cohort_Analysis_Period.R
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
#                         01b_Arrange_Vaccinations_Long.R to get df - the analysis data set
#                         Runs the coxph and data aggregation codes
# Approximate run time: Unknown
##########################################################

print(z_event_endpoint)
#df <- readRDS(paste0(project_path,"/output/temp/df_",z_event_endpoint,".RDS"))
print(table(df$event))
print(table(df$vacc_status, exclude=NULL))

z_combine_vacc_level <- TRUE  # to combine the first 1 or 2 levels of vacc_status with uv
output_list$combine_levels <- z_combine_vacc_level
z_adjustment <- "full" #or "minimal" "full
output_list$adjustment <- z_adjustment
output_list$prop_score <- "no propensity weighting" 
#"no propensity weighting"  "inverse propensity weighting"
output_list$model <- "glm" 
#"cox"  "glm"

data_selection_flags <- read.csv("02_data_selection_flags.csv")
data_selection_flags <- data_selection_flags %>% filter(!grepl("omit", notes))
source("00_Functions.R")
rm(z_res)

for (i in 1:nrow(data_selection_flags)) {
  
  #i <- 1
  
  
  z_df <- df
  
  if (z_combine_vacc_level){
    z_df <- z_df %>%  mutate(vacc_status = if_else(vacc_status %in% c("v1_0:6"), "uv", as.character(vacc_status))) 
    z_df <- z_df %>% 
      mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_7:13","v1_14:20",
                                                          "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) )
  }
  
  print(data_selection_flags[i,])
  #vacc_type
  if (data_selection_flags[i,"vacc_type"] == "PB") z_df <- filter(z_df, vacc_type %in% c("uv","PB") )
  if (data_selection_flags[i,"vacc_type"] == "AZ") z_df <- filter(z_df, vacc_type %in% c("uv","AZ") )
  #age
  z_df <- filter(z_df, ageYear >= data_selection_flags[i,"age_lower"] & ageYear <= data_selection_flags[i,"age_upper"]) 
  z_age_centre <- data_selection_flags[i,"age_lower"]
  if (z_age_centre < 30) z_age_centre <- 30
  #sex
  if (data_selection_flags[i,"sex"] %in% c("F","M")) z_df <- filter(z_df, Sex==data_selection_flags[i,"sex"])
  #previous positive tests
  if (data_selection_flags[i,"omit_prev_tests"] == "all") z_df <- filter(z_df, test_before_dec8 %in% c("no pos test","post-vacc") )
  if (data_selection_flags[i,"omit_prev_tests"] == "4w") z_df <- filter(z_df, !(test_before_dec8 %in% c("1+m")) )
  if (data_selection_flags[i,"omit_prev_tests"] == "2w") z_df <- filter(z_df, !(test_before_dec8 %in% c("1+m", "3w","4w")) )
  if (data_selection_flags[i,"omit_prev_tests"] == "1w") z_df <- filter(z_df, test_before_dec8 %in% c("0-6d","no pos test","post-vacc") )
  
  z_df$n_tests_gp <- cut(z_df$n_tests, breaks = c(-1,0,1,2,3,9,100), labels=c("0","1","2","3","4-9","10+"))
  
#z.yr <- tcut(rep(0,nrow(analysis.df)), c(-1,seq(7,as.numeric(a.analysis.to-a.analysis.from),by=7) ))
#aggregate for overall
z.agg <- pyears(Surv(start,stop,event) ~ vacc_status, data=z_df , weight=weight, scale=365.25, data.frame=TRUE)

df_res <- z.agg$data
names(df_res) <- c("Vacc.Status","Person.Years","Count","Events")
df_res <- df_res %>% mutate(Rate = Events/Person.Years*365.25,
                            Person.Years = round(Person.Years,0) ) %>% 
  mutate(RR = Rate/first(Rate))
df_res


z_var <- "vacc_status"

#plot(survfit(Surv(start, stop, event) ~  strata(period),data=z_df), fun="event")

#age adjustment
if (output_list$model == "cox") {
z <- coxph( Surv(start, stop, event) ~ get(z_var) + pspline(ageYear),data=z_df)
#z <- coxph( Surv(start, stop, event) ~ get(z_var) , data=z_df)
#print(summary(z))
z.estimates.1 <- fun.ve.cox("z_var",z)
}

if (output_list$model == "glm") {
 z.agg <- pyears(Surv(start,stop,event) ~ vacc_status + age_gp, data=z_df, weight=weight , scale=365.25, data.frame=TRUE)
 z_pois <- z.agg$data
 z_m <- glm(event ~ offset(log(pyears)) + vacc_status + age_gp, family=poisson, data=z_pois)
#summary(z_m)
 z.estimates.1 <- fun_ve_glm(z_m, z_type="")
}

if (z_adjustment == "minimal") { 

  #minimal adjustment
  if (output_list$model == "cox") {
    #z <- coxph( Surv(start, stop, event) ~ get(z_var) + pspline(ageYear) + Sex + simd2020_sc_quintile, data=z_df)
    z <- coxph( Surv(start, stop, event) ~ get(z_var) + pspline(ageYear, df=2) + Sex + simd2020_sc_quintile +
                  pspline(n_tests, df=2) + n_risk_gps + strata(period), data=z_df)
    #z <- coxph( Surv(start, stop, event) ~ period, data=z_df)
    #print(summary(z))
   z.estimates.2 <- fun.ve.cox("z_var",z)
   }
  
  if (output_list$model == "glm") {
    z.agg <- pyears(Surv(start,stop,event) ~ vacc_status + period + age_gp + simd2020_sc_quintile +
                  Sex + n_risk_gps +n_tests_gp, data=z_df, weight=weight , scale=365.25, data.frame=TRUE)
    z_pois <- z.agg$data
    z_m <- glm(event ~ offset(log(pyears)) + vacc_status + period + age_gp + simd2020_sc_quintile+
             Sex + n_risk_gps + n_tests_gp, family=poisson, data=z_pois)
    z.estimates.2 <- fun_ve_glm(z_m, z_type="")
    #summary(z_m)
    }
}  # end minimal

if (z_adjustment == "full") { 
  
  if (output_list$model == "cox") {
    z.fmla <- as.formula(paste("Surv(start, stop, event)"," ~  get(z_var) + strata(period) + ",
                               paste(variables_hosp, collapse= "+")))
    if (output_list$prop_score == "inverse propensity weighting")  {
          z <- coxph(z.fmla, data = z_df, weight=inv_psw)
    } #end cox ipw
    if (output_list$prop_score == "no propensity weighting")  {
         z <- coxph(z.fmla, data = z_df, weight=eave_weight)
    } #end cox no ipw
    
    z.estimates.2 <- fun.ve.cox("z_var",z)
    
  } # end cox
  
  if (output_list$model == "glm") {
    z_pois <- z_df %>% mutate(pyears=stop-start)
    z.fmla <- as.formula(paste("event"," ~ offset(log(pyears)) + vacc_status + period + ",
                             paste(variables_hosp, collapse= "+")))
    if (output_list$prop_score == "inverse propensity weighting")  {
            z_m <- glm(formula = z.fmla  , data=z_pois, family=poisson, weight = inv_psw)
      } # end glm ipw
    
    if (output_list$prop_score == "no propensity weighting")  {
    z_m <- glm(formula = z.fmla  , data=z_pois, family=poisson, weight = eave_weight)
      } # end glm no ipw
  #summary(z_m)
    z.estimates.2 <- fun_ve_glm(z_m, z_type="")
  } # end glm



#ptemp <- termplot(z, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==z_age_centre])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#p_df <- data.frame(x=ageterm$x, exp(ytemp - center)) %>% dplyr::rename(hr=X1, lcl=X2, ucl=X3)
#saveRDS(p_df,paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_hr_plot_age_",i,".RDS"))

#p_df %>% ggplot(aes(x=x)) + geom_line(aes(y=hr)) + geom_line(aes(y=lcl), linetype=2) +
#  geom_line(aes(y=ucl), linetype=2) + geom_hline(yintercept=1) + scale_y_log10() +
#  labs(x="Age",y="Hazard Ratio")

#ageterm <- ptemp$n_tests  # this will be a data frame
#center <- with(ageterm, y[x==0])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#p_df <- data.frame(x=ageterm$x, exp(ytemp - center)) %>% dplyr::rename(hr=X1, lcl=X2, ucl=X3)
#saveRDS(p_df,paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_hr_plot_tests_",i,".RDS"))

#p_df %>% ggplot(aes(x=x)) + geom_line(aes(y=hr)) + geom_line(aes(y=lcl), linetype=2) +
#  geom_line(aes(y=ucl), linetype=2) + geom_hline(yintercept=1) + scale_y_log10() +
#  labs(x="Number of Tests",y="Hazard Ratio")

} #end full adjustment 

#merge the summary data and estimates
z_names_or <- c("vacc_status","OR","OR_LCL","OR_UCL")
z_out <- df_res %>% left_join(z.estimates.1[,z_names_or], by=c("Vacc.Status" = "vacc_status")) %>% 
  left_join(z.estimates.2[,z_names_or], by=c("Vacc.Status" = "vacc_status"), suffix = c("_raw", "_adj"))

z <- data_selection_flags[i,]
for (i_row in 2:nrow(z_out)) z <- bind_rows(z, data_selection_flags[i,])

z_out <- bind_cols(z,z_out)

z_out$selections <- paste(output_list[[1]] , output_list[[5]], output_list[[6]], output_list[[8]])

if (exists("z_res")) z_res <- bind_rows(z_res,z_out) else z_res <- z_out
} # end loop through the models

names(z_res) <- gsub("^OR_", "HR_", names(z_res))
names(z_res) <- gsub("^HR_LCL_", "LCL_", names(z_res))
names(z_res) <- gsub("^HR_UCL_", "UCL_", names(z_res))
output_list$results <- z_res
saveRDS(output_list,paste0(project_path,"/output/temp/CR_cohort_summary_",z_event_endpoint,".RDS"))
write_csv(output_list$results, paste0(project_path,"/output/temp/CR_cohort_summary_",z_event_endpoint,".csv"))
