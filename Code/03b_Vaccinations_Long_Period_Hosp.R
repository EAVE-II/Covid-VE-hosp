
#z_df is the data set for analysis
#z_ids are the id nos from z_df

z_covariates <- z_df

z_vacc <- z_vaccinations %>% filter(EAVE_LINKNO %in% z_ids)

z_ids_unvacc <- z_ids[!(z_ids %in% z_vacc$EAVE_LINKNO)]

z_days <- data.frame(Date=seq.Date(a_begin,a_end, by="days"))
z_id <- z_vacc %>% dplyr::select(EAVE_LINKNO, date_vacc_1, date_vacc_2)

z <- full_join(z_id, z_days, by=character())
z_seq_dates <- seq.Date(a_begin, a_end, by=7)
z_seq_dates[length(z_seq_dates)] <- a_end+1
z <- z %>% 
  mutate(day_1 = as.numeric(Date - date_vacc_1),
        day_2 = as.numeric(Date - date_vacc_2)) %>% 
  mutate(vacc_1_gp = cut(day_1, breaks= c((min(day_1, na.rm=T)-1), 0, 6, 13, 20, 27, 34, 41, max(day_1, na.rm=T)),
                           labels=c("uv","v1_0:6","v1_7:13","v1_14:20","v1_21:27","v1_28:34","v1_35:41", "v1_42+")),
         vacc_2_gp = cut(day_2, breaks= c((min(day_2, na.rm=T)-1), 0, 6,  max(day_2, na.rm=T)) ,
                           labels=c("v2_uv" , "v2_0:6","v2_7+")) ) %>% 
  mutate(vacc_status = as.character(vacc_1_gp)) %>% 
  mutate(vacc_status = case_when( !is.na(vacc_2_gp)&vacc_2_gp != "v2_uv"  ~ as.character(vacc_2_gp),
                                  TRUE ~ vacc_status)) %>% 
  mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_0:6","v1_7:13","v1_14:20",
                              "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) ) %>% 
  mutate(period=cut(Date, breaks = z_seq_dates, include_lowest=TRUE))

#now add in an indicator to denote if the person is in hospital for any reason
#this file has all admissions and discharges - it comes from Bob Taylor's work on
#tidying up rapid to correct for anomalies in discharge dates
#select all people in hospital post dec 08
#replace missing discharge dates with the end of study date to calculate the intervals
z_hosp <- hosp_adm_nov01 %>% filter (is.na(discharge_date) | discharge_date >= a_begin) %>% 
  mutate(discharge_date = if_else(is.na(discharge_date), a_end, discharge_date))%>% 
  filter(EAVE_LINKNO %in% unique(z_id$EAVE_LINKNO)) #omit individuals not in the selected data

#arrange the data into intervals with one row per person
df_intervals <- z_hosp %>% 
  mutate(intervals = interval(start = admission_date, end = discharge_date)) %>% 
  select(EAVE_LINKNO, intervals) %>% 
  group_by(EAVE_LINKNO) %>% 
  mutate(interval_num = row_number()) %>% 
  pivot_wider(id_cols = EAVE_LINKNO, values_from = intervals, names_from = interval_num, names_prefix = "adm_")

#join the hospital stays to the long data by day
z <- z %>%  left_join(df_intervals, by="EAVE_LINKNO") %>% 
  mutate(across(starts_with("adm_"), ~lubridate::`%within%`(Date, .))) %>% # TRUE/FALSE whether this date intersects a particular stay
  mutate(across(starts_with("adm_"), ~replace_na(., FALSE))) %>% 
  rowwise() %>% 
  mutate(in_hosp = sum(c_across(starts_with("adm_")))) %>% # should just be 0s or 1s - 2s is there is a discharge and admission on the same day
  mutate(in_hosp= if_else(in_hosp>1,1L,in_hosp) ) %>% 
  select(-starts_with("adm_"))
z <- z %>% dplyr::select(-starts_with("adm_"))

z_wide <- z %>% mutate(date = lubridate::ymd(Date)) %>% 
  group_by(EAVE_LINKNO, period, vacc_status) %>% 
  dplyr::summarise(start = min(date), stop=max(date), in_hosp=sum(in_hosp)) %>% ungroup()
#z_wide is the stop and start values for the vaccinated
#with time since vaccination and weeks since a_begin as the grouping factors

#repeat for the unvaccinated but without the vaccination split
z_unvacc <- data.frame(EAVE_LINKNO = z_ids_unvacc) %>% mutate(vacc_status = "uv") 
z <- full_join(z_unvacc, z_days, by=character())%>% 
  mutate(period=cut(Date, breaks = z_seq_dates, include_lowest=TRUE))

#now add in an indicator to denote if the person is in hospital for any reason
#this file has all admissions and discharges - it comes from Bob Taylor's work on
#tidying up rapid to correct for anomalies in discharge dates
#select all people in hospital post dec 08
#replace missing discharge dates with the end of study date to calculate the intervals
z_hosp <- hosp_adm_nov01 %>% filter (is.na(discharge_date) | discharge_date >= a_begin) %>% 
  mutate(discharge_date = if_else(is.na(discharge_date), a_end, discharge_date))%>% 
  filter(EAVE_LINKNO %in% unique(z_unvacc$EAVE_LINKNO)) #omit individuals not in the selected data

#arrange the data into intervals with one row per person
df_intervals <- z_hosp %>% 
  mutate(intervals = interval(start = admission_date, end = discharge_date)) %>% 
  select(EAVE_LINKNO, intervals) %>% 
  group_by(EAVE_LINKNO) %>% 
  mutate(interval_num = row_number()) %>% 
  pivot_wider(id_cols = EAVE_LINKNO, values_from = intervals, names_from = interval_num, names_prefix = "adm_")

#join the hospital stays to the long data by day
z <- z %>%  left_join(df_intervals, by="EAVE_LINKNO") %>% 
  mutate(across(starts_with("adm_"), ~lubridate::`%within%`(Date, .))) %>% # TRUE/FALSE whether this date intersects a particular stay
  mutate(across(starts_with("adm_"), ~replace_na(., FALSE))) %>% 
  rowwise() %>% 
  mutate(in_hosp = sum(c_across(starts_with("adm_")))) %>% # should just be 0s or 1s - 2s is there is a discharge and admission on the same day
  mutate(in_hosp= if_else(in_hosp>1,1L,in_hosp) ) %>% 
  select(-starts_with("adm_"))
z <- z %>% dplyr::select(-starts_with("adm_"))

z_res <- z %>% mutate(date = lubridate::ymd(Date)) %>% 
  group_by(EAVE_LINKNO,  period, vacc_status) %>% 
  dplyr::summarise(start = min(date), stop=max(date), in_hosp=sum(in_hosp)) %>% ungroup()
#z_res is the stop and start values for the unvaccinated
#with weeks since a_begin as the grouping factor
z_res <- z_res %>% dplyr::relocate(period, .before=vacc_status)

#z_unvacc <- data.frame(EAVE_LINKNO = z_ids_unvacc) %>% mutate(period=a_begin, vacc_status = "uv", start = a_begin, stop=a_begin+6) 
#z_res <- z_unvacc
#for (i in 2 : floor(as.numeric((a_end-a_begin))/7)) {
#  z_unvacc <- z_unvacc %>% mutate(start=start+7, stop=stop+7, period=period+7)
#  z_res <- bind_rows(z_res, z_unvacc)}
#z_unvacc <- z_unvacc %>% mutate(start=start+7, stop=a_end)
#z_res <- bind_rows(z_res, z_unvacc)
#z_res <- z_res %>%  mutate(period = as.factor(period))
#z_res is the stop and start values for the unvaccinated
#with time since vaccination and weeks since a_begin as the grouping factors

z_vaccination_long <- bind_rows(z_wide,z_res) %>% 
  arrange(EAVE_LINKNO, start) %>%
  mutate(start = start-1) %>%  # subtract 1 so that intervals are of correct length
  mutate(start = if_else(start< a_begin,a_begin,start)) %>% 
  filter(stop > start) 
# all intervals should have stop > start  the omitted are vaccinated on a_begin
# as all events start from a_begin+1

z_vaccination_long <- z_vaccination_long %>% 
  mutate(vacc_status = factor(vacc_status, levels = c("uv","v1_0:6","v1_7:13","v1_14:20",
                          "v1_21:27","v1_28:34","v1_35:41", "v1_42+", "v2_0:6","v2_7+")) )
  
