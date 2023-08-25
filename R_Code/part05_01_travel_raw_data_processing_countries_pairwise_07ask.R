# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.11. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen = 20)

# Library to perform column medians and other useful matrix algebra computations. 
#library(matrixStats)

# Library for the latex exports in the nice format.
#library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
#library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
#library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files. 
# library(rjson)

# Library for read_csv
#library(readr)

# To convert dates from text to dates
#library(lubridate)

# Loading package required to read library(readxl)
library(readxl)

# Library for permutations
library(gtools)

# This package is used to produce latex tables.
# library(xtable)


# Reading data
load(file = paste("../R_Data/countries.RData"))
# Fix 2023.03.01.
# Loading the country's population data.
load(file = paste("../R_Data/population_data_selected.RData"))
# Fix 2022.10.31 ask
# Excluding Serbia and Montenegro since they were not in the travel dataset.
countries_travel <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]
save(countries_travel, file = paste("../R_Data/countries_travel.RData"))



# Path for the data
travel_data_countries_path <- "../Data/Travel/KCMD_DDH_data_KCMD-EUI GMP_ Estimated trips.xlsx"
travel_meta_countries_path <- "../Data/Travel/KCMD_DDH_meta_KCMD-EUI GMP_ Estimated trips.xlsx"
# incidence_data_all_path = "Data/time_series_covid19_confirmed_global.csv"
# mortality_data_all_path = "Data/time_series_covid19_deaths_global.csv"

# Reading data
travel_data_countries_raw <- data.frame(read_excel(path = travel_data_countries_path))
travel_meta_countries_raw <- data.frame(read_excel(path = travel_meta_countries_path))
dim(travel_data_countries_raw)
dim(travel_meta_countries_raw)


# Fixing Bosnia and Herzegovina and Czechia
# sort(travel_meta_countries_raw$country)
travel_meta_countries_raw$country[ travel_meta_countries_raw$country == "Bosniaand Herzegovina" ] <- "Bosnia and Herzegovina"
travel_meta_countries_raw$country[ travel_meta_countries_raw$country == "Czech Republic" ]        <- "Czechia"


# Fix 2022.11.10
# Exporting countries in the list -> countries
# Subsetting
travel_meta_short <- data.frame(travel_meta_countries_raw[ travel_meta_countries_raw$country %in% countries, ])
travel_meta_long  <- rbind(travel_meta_short, c("SRB", "Serbia"),  c("MNE", "Montenegro") )
travel_meta_countries <- travel_meta_long[order(travel_meta_long$country), ] 
dim(travel_meta_countries)
# Saving
save(travel_meta_countries, file = paste("../R_Data/travel_meta_countries.RData"))



# Subsetting
travel_meta_countries_eu <- data.frame(travel_meta_countries_raw[ travel_meta_countries_raw$country %in% countries_travel, ])
# Extra check
dim(travel_meta_countries_eu)[1] == length(countries_travel)


# Merging to the original dataset
# Keeping only countried_travel as source
names(travel_data_countries_raw)
names(travel_meta_countries_eu)
travel_merge_countries_raw <- merge( x = travel_data_countries_raw, y = travel_meta_countries_eu, by.x = "reporting.country", by.y = "code" )
dim(travel_data_countries_raw)
dim(travel_merge_countries_raw)
head(travel_data_countries_raw)
head(travel_merge_countries_raw)
# Keeping only countried_travel as destination
travel_merge_countries_eu <- travel_merge_countries_raw[ travel_merge_countries_raw$secondary.country %in% travel_meta_countries_eu$code, ]
dim(travel_merge_countries_eu)
head(travel_merge_countries_eu)

# Extending with the second country.
travel_merge_countries_eu_extedned <- merge( x = travel_merge_countries_eu, y = travel_meta_countries_eu, by.x = "secondary.country", by.y = "code" )
head(travel_merge_countries_eu)
head(travel_merge_countries_eu_extedned)
dim(travel_merge_countries_eu)
dim(travel_merge_countries_eu_extedned)
# fixing names
names(travel_merge_countries_eu_extedned)[5] <- "Country1"
names(travel_merge_countries_eu_extedned)[6] <- "Country2"



# Creating a frame with results.

# Creating a frame of pairs
pairs_frame <- combinations(n = length(countries_travel), r = 2, v = countries_travel, repeats.allowed = FALSE)

# Creating frame for distances
pairs_frame_travel <- data.frame( pairs_frame, travel_statistic_2016 = rep(0, dim(pairs_frame)[1]), 
                                               travel_statistic_average_2011_2016 = rep(0, dim(pairs_frame)[1]), 
                                               last_reported_year = rep(0, dim(pairs_frame)[1]), 
                                               first_reported_year = rep(0, dim(pairs_frame)[1]) )
names(pairs_frame_travel)[c(1, 2)] <- c("Country1", "Country2")


# Creating RAW frame for distances
pairs_frame_travel_raw <- data.frame( pairs_frame, travel_statistic_2016 = rep(0, dim(pairs_frame)[1]), 
                                      travel_statistic_average_2011_2016 = rep(0, dim(pairs_frame)[1]), 
                                      last_reported_year = rep(0, dim(pairs_frame)[1]), 
                                      first_reported_year = rep(0, dim(pairs_frame)[1]) )
names(pairs_frame_travel)[c(1, 2)] <- c("Country1", "Country2")



for (current_index in c(1:dim(pairs_frame_travel)[1])) {
  
  # Debugging step
  # current_index = 1 

  # Extracting country names for the current pair.
  country1_indexes <- which( travel_merge_countries_eu_extedned$Country1 == pairs_frame_travel$Country1[current_index] | 
                             travel_merge_countries_eu_extedned$Country1 == pairs_frame_travel$Country2[current_index] )
  country2_indexes <- which( travel_merge_countries_eu_extedned$Country2 == pairs_frame_travel$Country2[current_index] |
                             travel_merge_countries_eu_extedned$Country2 == pairs_frame_travel$Country1[current_index] )
  country_indexes_intercept <- intersect(country1_indexes, country2_indexes)
  
  # Current Frame work
  current_countries_frame <- travel_merge_countries_eu_extedned[country_indexes_intercept, ]
  
  # Fix 2023.03.02.
  reporting_counties_code <- unique(current_countries_frame$reporting.country)
  
  # Standardization for reporting_counties_list[1] and reporting_counties_list[2]
  reporting_country1 <- travel_meta_short$country[which( travel_meta_short$code == reporting_counties_code[1] )]
  reporting_country2 <- travel_meta_short$country[which( travel_meta_short$code == reporting_counties_code[2] )]
  
  
  # Raw Data.

  # Extracting the summaries of interest
  current_countries_frame_most_recent  <-  sum( current_countries_frame[ which(current_countries_frame$time == max(current_countries_frame$time)), "value" ] )
  current_countries_frame_least_recent <-  sum( current_countries_frame[ which(current_countries_frame$time == min(current_countries_frame$time)), "value" ] )
  current_countries_frame_average      <-  mean( aggregate(value ~ time, current_countries_frame, sum)$value )

  # Years range
  time_range <- range( aggregate(value ~ time, current_countries_frame, sum)$time )

  # Saving the results into the frame
  pairs_frame_travel_raw$travel_statistic_2016[current_index]              <- current_countries_frame_most_recent
  pairs_frame_travel_raw$travel_statistic_average_2011_2016[current_index] <- current_countries_frame_average
  pairs_frame_travel_raw$first_reported_year[current_index] <- time_range[1]
  pairs_frame_travel_raw$last_reported_year[current_index]  <- time_range[2]

  
  
  # standartization process.
  
  # Getting total population for country1 and country2.
  total_population_both_countries <- 
    population_data_selected[population_data_selected$Country == reporting_country1, "Counts2020" ] +
    population_data_selected[population_data_selected$Country == reporting_country2, "Counts2020" ]
  

  current_countries_frame[ (current_countries_frame$reporting.country == reporting_counties_code[1]), "value" ] <- 
    100000 * current_countries_frame[ (current_countries_frame$reporting.country == reporting_counties_code[1]), "value" ]/ 
    total_population_both_countries 
  
  current_countries_frame[ (current_countries_frame$reporting.country == reporting_counties_code[2]), "value" ] <- 
    100000 * current_countries_frame[ (current_countries_frame$reporting.country == reporting_counties_code[2]), "value" ]/ 
    total_population_both_countries  
  
  
  
  # Extracting the summaries of interest
  current_countries_frame_most_recent  <-  sum( current_countries_frame[ which(current_countries_frame$time == max(current_countries_frame$time)), "value" ] )
  current_countries_frame_least_recent <-  sum( current_countries_frame[ which(current_countries_frame$time == min(current_countries_frame$time)), "value" ] )
  current_countries_frame_average      <-  mean( aggregate(value ~ time, current_countries_frame, sum)$value )
  
  # Years range
  time_range <- range( aggregate(value ~ time, current_countries_frame, sum)$time )
  
  # Saving the results into the frame
  pairs_frame_travel$travel_statistic_2016[current_index]              <- current_countries_frame_most_recent
  pairs_frame_travel$travel_statistic_average_2011_2016[current_index] <- current_countries_frame_average
  pairs_frame_travel$first_reported_year[current_index] <- time_range[1]
  pairs_frame_travel$last_reported_year[current_index]  <- time_range[2]
  
  
    # cat("Iteration Finished -> ", current_index, "\n")
  # End of -> for ( current_index in c(1:dim(pairs_frame_travel)[1]) )
}

# Checking summaries recent year
summary(pairs_frame_travel$last_reported_year)
summary(pairs_frame_travel$first_reported_year)



# Saving the processed results as RData files.
# travel
save(pairs_frame_travel, file = paste("../R_Data/pairs_frame_travel.RData"))




# Saving the processed results as CSV files.
# travel raw
pairs_frame_travel_raw_csv_path = paste("../R_Output/pairs_frame_travel_raw.csv", sep = "")
write.table(x = pairs_frame_travel_raw, file = pairs_frame_travel_raw_csv_path, sep = ",", row.names = FALSE)

# travel
pairs_frame_travel_csv_path = paste("../R_Output/pairs_frame_travel.csv", sep = "")
write.table(x = pairs_frame_travel, file = pairs_frame_travel_csv_path, sep = ",", row.names = FALSE)

