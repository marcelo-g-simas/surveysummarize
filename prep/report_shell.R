library(hatstats)
library(hatviz)

config_file <- system.file('studies/NIRPC HTS.yaml', package = 'hatstats')
hts_data <- read_data(config_file)
# hts_data <- read_data(config = "study_config.yaml")

#================================================================================================================================================#
#### Tables ####
#================================================================================================================================================#

#=================================================================================#
# Table 1.	Distribution of travel dates =====

tbl1 <- summarize_data(
  hts_data,
  agg = 'household_count',
  by = 'travdate'
)

tbl1[, N := NULL]

make_table(tbl1, confidence = 0.95)

# tbl1_html <- make_table(tbl1, confidence = 0.95)
# html_table_to_docx(tbl1_html, 'test.docx')

#=================================================================================#
# Table 2.	Sample Distributions =====



#=================================================================================#
# Table 3.	Data Collection Process Description =====



#=================================================================================#
# Table 4.	Data Collection Process Description (Conitnued) =====



#=================================================================================#
# Table 5. 	Data Elements Collected =====



#=================================================================================#
# Table 6.	Item Non-response for Recruitment Questions =====



#=================================================================================#
# Table 7.	Item Non-response for Retrieval/Travel Characteristic Questions =====



#=================================================================================#
# Table 8.	Stage 2 QA/QC Checks for Household and Person Characteristics =====



#=================================================================================#
# Table 9.	Stage 2 QA/QC Checks for Place Characteristics =====



#=================================================================================#
# Table 10.	Household Size by Number of Household Workers =====

tbl10 <- summarize_data(
  data = hts_data,
  agg = 'household_count',
  by = c('hhsize_agg','hhworker_agg')
)

make_table(tbl10, row_vars = c('hhsize_agg','hhworker_agg'))

#=================================================================================#
# Table 11.	Household Size by Number of Household Vehicles =====




#=================================================================================#
# Table 12.	Number of Workers by Number of Household Vehicles =====



#=================================================================================#
# Table 13.	Household Income; Surveyed and Weighted versus ACS =====



#=================================================================================#
# Table 14.	Person Characteristics; Surveyed and Weighted versus ACS =====



#=================================================================================#
# Table 15.	Household Characteristics; Surveyed and Weighted versus ACS =====



#=================================================================================#
# Table 16.	Summary Survey Results =====



#=================================================================================#
# Table 17.	Household Size =====



#=================================================================================#
# Table 18.	Household Vehicles =====



#=================================================================================#
# Table 19.	Household Workers =====



#=================================================================================#
# Table 20.	Household Income =====



#=================================================================================#
# Table 21.	Home Ownership =====



#=================================================================================#
# Table 22.	Respondent Race =====



#=================================================================================#
# Table 23.	Respondent Age =====



#=================================================================================#
# Table 24.	Household Trip Rates =====

tbl24 <- summarize_data(
  data = hts_data,
  agg = 'household_trip_rate'
)

#=================================================================================#
# Table 25.	Household Trip Rates by Household Size by Household Vehicles =====



#=================================================================================#
# Table 26.	Household Trip Rates by Household Size =====



#=================================================================================#
# Table 27.	Household Trip Rates by Household Vehicles =====



#=================================================================================#
# Table 28.	Household Trip Rates by Household Workers =====



#=================================================================================#
# Table 29.	Household Trip Rates by Household Income =====



#=================================================================================#
# Table 30.	Household Trip Rates by Home Ownership =====



#=================================================================================#
# Table 31.	Person Trip Rates =====



#=================================================================================#
# Table 32.	Person Trip Rates by Gender =====



#=================================================================================#
# Table 33.	Person Trip Rates by Age =====



#=================================================================================#
# Table 34.	Person Trip Rates by Race =====



#=================================================================================#
# Table 35.	Person Trip Rates by Driver’s License Status =====



#=================================================================================#
# Table 36.	Person Trip Rates by Person Type =====



#=================================================================================#
# Table 37.	Trip Type Definitions Used in Analysis =====



#=================================================================================#
# Table 38.	Frequency, Trip Rate, Average Minutes, and Average Distances by Trip Types =====



#=================================================================================#
# Table 39.	Mode Share =====



#=================================================================================#
# Table 40.	Average Trip Duration (in minutes) by Mode =====

tbl40 <- summarize_data(
  data = hts_data,
  agg = 'avg',
  agg_var = 'travtime',
  by = 'mode'
)


#=================================================================================#
# Table 41.	Average Trip Distance (in miles) by Mode =====



#=================================================================================#
# Table 42.	Actual versus Typical Work Mode =====



#=================================================================================#
# Table 43.	Actual versus Typical School Mode =====



#=================================================================================#

#================================================================================================================================================#
#### Figures ####
#================================================================================================================================================#

# Figure 3.	Study Area with Home Locations for Final Delivered Households =====



#=================================================================================#
# Figure 4.	Study Area with Home Locations for Final Delivered Households =====



#=================================================================================#
# Figure 5. 	Representation of Captured GPS points from the Daily Travel Apps =====



#=================================================================================#
# Figure 6.	Volume of Household Trips =====



#=================================================================================#
# Figure 7.	Person Types =====



#=================================================================================#
# Figure 8.	Departure Times by Time of Day =====



#=================================================================================#
# Figure 9.	Departure Times – Raw Counts (weighted counts divided by 1000) =====



