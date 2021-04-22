#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

#output_path <- "F:/PHI/HPS-OA/Collaborative Working/2019-nCoV/01 Surveillance and investigation/Chris analysis/Maps"
# output_path <- "Z:/StatsSupport/Coronavirus/Routine_surveillance_reporting/LA_Exception_Reporting"
output_path <- getwd()
z_event_endpoint <- "hosp_covid"
a.today <- Sys.Date()
outfile <- paste0("Test_", format(a.today,"%d%m%Y"), ".html")
doc_title <- "Local Authority Exceedance Charts - All Ages"
switch_age <- "all"  #all ages
rmarkdown::render(input = "03c_Cohort_Results.Rmd", output_format ="html_document", output_file = outfile,
                  output_dir = output_path )


#for (switch_age in c("children", "student", "adult")){ 
#  #switch_age <- "adult" # "children", "student"
#  outfile <- paste0("ES_LA_",switch_age,"_", format(a.today,"%d%m%Y"), ".html")
#  doc_title <- paste0("Local Authority Exceedance Charts - ",switch_age)
  
#  rmarkdown::render(input = "Exception_System_LA.Rmd", output_format ="html_document", output_file = outfile,
#                    output_dir = output_path )
#}

