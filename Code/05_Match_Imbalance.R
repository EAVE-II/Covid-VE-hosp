##########################################################
# Name of file: 05_Match_Imbalance.R
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
#                         matching on Age, Sex plus - to change the variables the list has to be changed in a number of places
#                         Run 05_Match_vacc.R to get the matched study
#                         Compares the covariate imbalance over the vacc and unvacc groups
# Approximate run time: Unknown
##########################################################

#study data set is df_cc
print(output_list$event)


z_df <- df_cc %>%
  dplyr::select(Sex, age_gp,simd2020_sc_quintile, ur6_2016_name, n_risk_gps, n_tests_gp,  vacc, vacc_type) %>% 
  pivot_longer(cols=Sex:n_tests_gp) 

z <- z_df %>% group_by(name, value) %>% 
  dplyr::summarise(N=n(), Vaccinated = sum(vacc=="vacc"), 
                   AZ = sum(vacc_type=="AZ"), PB = sum(vacc_type=="PB")) %>% 
  mutate(Unvaccinated=N-Vaccinated) %>% ungroup()
#z <- z %>%  group_by(name) %>% mutate(RR=Uptake/first(Uptake)) %>% ungroup() %>% as.data.frame()
z2 <- z %>% group_by(name) %>% dplyr::summarise(N_U = sum(Unvaccinated), N_V=sum(Vaccinated), 
                                                N_AZ = sum(AZ), N_PB=sum(PB))
z <- z %>% left_join(z2, by="name") %>% 
  mutate(Percent_of_Vaccinated = round(Vaccinated/N_V*100,1), 
         Percent_of_Unvaccinated = round(Unvaccinated/N_U*100,1),
         Percent_of_AZ = round(AZ/N_AZ*100,1),
         Percent_of_PB = round(PB/N_PB*100,1)  )
z <- z %>% dplyr::select(name, value, Vaccinated, Percent_of_Vaccinated, Unvaccinated, Percent_of_Unvaccinated,
                         AZ, Percent_of_AZ, PB, Percent_of_PB) %>% ungroup() %>% as.data.frame()


z_df <- df_cc %>%
  dplyr::select(Q_DIAG_AF:Q_DIAG_VTE,vacc, vacc_type) %>% 
  pivot_longer(cols=Q_DIAG_AF:Q_DIAG_VTE)

#run code above to get z
#as all binary just select the 1's
z_imb  <- z %>%  filter(value==1) %>% dplyr::select(name, Percent_of_Vaccinated, Percent_of_Unvaccinated)
z_uv <- sum(df_cc$vacc=="uv")
z_vacc <- sum(df_cc$vacc=="vacc")
z_imb <- z_imb %>% mutate(diff = Percent_of_Vaccinated - Percent_of_Unvaccinated) %>% 
  mutate(prop_vacc = Percent_of_Vaccinated/100, prop_uv = Percent_of_Unvaccinated/100) %>% 
  mutate(var_vacc = prop_vacc*(1-prop_vacc)/z_vacc, 
         var_uv = prop_uv*(1-prop_uv)/z_uv) %>% 
  mutate(std_diff = (diff/100)/sqrt(var_vacc + var_uv))

z_df <- df%>%
  ls()dplyr::select(EAVE_Smoke, EAVE_BP,           vacc, vacc_type, eave_weight) %>% 
  pivot_longer(cols=-(vacc:eave_weight))

