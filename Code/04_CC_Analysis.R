##########################################################
# Name of file: 04a_CC_Analysis.R
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
#                         run 04_CC.R to set up the cc study
#                         matching on Age, Sex
#                         this script fits the models
# Approximate run time: Unknown
##########################################################

print(z_event_endpoint)
#df_cc <- readRDS(paste0(project_path,"/output/temp/df_cc_",z_event_endpoint,".RDS"))
print(table(df_cc$event))

z_combine_vacc_level <- FALSE # to combine the first 1 or 2 levels of vacc_status with uv

data_selection_flags <- read.csv("02_data_selection_flags.csv")
data_selection_flags <- data_selection_flags %>% filter(!grepl("omit", notes))
source("00_Functions.R")
rm(z_res)

for (i in 1:nrow(data_selection_flags)) {
  
  #i <- 2
  
  
  z_df <- df_cc
  
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
  
#omit those in hospital
z_df <- z_df %>% filter(hosp!="in_hosp") %>% mutate(hosp=factor(hosp))
   
df_res <- z_df %>% group_by(vacc_status) %>% 
  dplyr::summarise(N=n(), R=sum(event)) %>% 
  mutate(Percent=R/N*100)
df_res 

z_var <- "vacc_status"
z_df$vacc_status <- factor(z_df$vacc_status)
z_fit <- clogit( event ~ get(z_var) + strata(EAVE_LINKNO_case), data=z_df )
summary(z_fit)
z.estimates.1 <- fun.ve.cox("z_var",z_fit)


z_fit <- clogit( event ~ get(z_var) + strata(EAVE_LINKNO_case) + hosp + simd2020_sc_quintile,
                   data=z_df )
summary(z_fit)
z.estimates.2 <- fun.ve.cox("z_var",z_fit)

#merge the summary data and estimates

z_out <- df_res %>% left_join(z.estimates.1, by=c("vacc_status" = "var")) %>% 
  left_join(z.estimates.2, by=c("vacc_status" = "var"), suffix = c("_raw", "_adj"))

z <- data_selection_flags[i,]
for (i_row in 2:nrow(z_out)) z <- bind_rows(z, data_selection_flags[i,])

z_out <- bind_cols(z,z_out)

if (exists("z_res")) z_res <- bind_rows(z_res,z_out) else z_res <- z_out
}


saveRDS(z_res,paste0(project_path,"/output/temp/CR_cc_summary_",z_event_endpoint,".RDS"))
