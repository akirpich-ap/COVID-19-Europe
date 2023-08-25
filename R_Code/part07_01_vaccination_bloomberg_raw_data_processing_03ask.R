# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.03. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations.
library(matrixStats)

# Library for the latex exports in the nice format.
library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
library(readr)

# To convert dates from text to dates
library(lubridate)



# Loading countries names from part05_01 file
load(file = paste("../R_Data/travel_meta_countries.RData"))
# Loading countrues population.
load(file = paste("../R_Data/population_data_selected.RData"))
dim(population_data_selected)
load(file = paste("../R_Data/countries.RData"))


# Fix 2023.06.02.
# Specifying the aggregation period before standardization.
aggregation_periods_in_days <- 7
# Rates per counts
rates_per_counts <- 100000
# Subsetting up to Feb 15
subsetting_end_date <- as.Date(x = "2022-02-15", origin = "1970-01-01" ) 




# Path for the vaccination data from Bloomberg
# https://github.com/BloombergGraphics/covid-vaccine-tracker-data
# File path
vaccination_data_all_path = "https://raw.githubusercontent.com/BloombergGraphics/covid-vaccine-tracker-data/master/data/historical-global-doses-administered.csv"


# Reading data
vaccination_data_all_raw <- read.table(file  = vaccination_data_all_path, header = TRUE, sep = ",")
dim(vaccination_data_all_raw)
head(vaccination_data_all_raw)
# Fixing dates
is.Date(vaccination_data_all_raw$date)
vaccination_data_all_raw$date <- as.Date(vaccination_data_all_raw$date)



# Fix 2022.04.05.
# Subsetting up to subsetting_end_date defined at the top
which_incidence_column_subsetting_end_date <- which( as.Date(vaccination_data_all_raw$date) <= subsetting_end_date )
vaccination_data_all_raw_subset_date <- vaccination_data_all_raw[which_incidence_column_subsetting_end_date, ]
# Extra checks
dim(vaccination_data_all_raw)
dim(vaccination_data_all_raw_subset_date)


# Performing inner merge
names(vaccination_data_all_raw_subset_date)
names(travel_meta_countries)
vaccination_data_all <- merge( x = vaccination_data_all_raw_subset_date, y = travel_meta_countries, by.x = "id", by.y = "code" )
dim(vaccination_data_all)
head(vaccination_data_all)
unique(vaccination_data_all$id)
unique(vaccination_data_all$country)

# Saving 
save(vaccination_data_all, file = paste("../R_Data/vaccination_data_all.RData"))



# Determining matrix dimensions
range(vaccination_data_all$date)[2] - range(vaccination_data_all$date)[1] 

# Dates range
date_formatted <- as.Date( range(vaccination_data_all$date)[1]:range(vaccination_data_all$date)[2], origin = "1970-01-01" ) 
# Determining columns
dat_matrix_raw <- matrix( data = 0, nrow = length(date_formatted), ncol = length(travel_meta_countries$country)  )
dim(dat_matrix_raw)


# Fix 2023.06.01.
# Creating an interpolated version to avoid missing values.

# Creating a frame
vaccination_data_interpolated <- data.frame( date = date_formatted, dat_matrix_raw )
names(vaccination_data_interpolated)[-1] <- sort(travel_meta_countries$country)


# Filling the frame!
for ( current_country in travel_meta_countries$country )
{
  # Debuging
  # current_country <- travel_meta_countries$country[1]
  # current_country <- travel_meta_countries$country[2]
  
  # Extracting the data for the given country
  current_country_data       <- vaccination_data_all[ which(vaccination_data_all$country == current_country),  ]

  # Sorting data frame by date
  current_country_data <-  current_country_data[ order(current_country_data$date), ]
  
  
  # Generating linear interpolator
  current_country_data_interpolator <- approx(x = current_country_data$date, y = current_country_data$value, rule = 2,
     xout = as.Date(c(min(current_country_data$date):max(vaccination_data_interpolated$date)), origin = "1970-01-01") )
  
  length(current_country_data_interpolator$x)
  length(current_country_data_interpolator$y)
  length(current_country_data$date)
  
  # Filling the values
  
  # Indexes for available dates
  which_interpolated_indexes <- which(vaccination_data_interpolated$date %in% current_country_data_interpolator$x)
  # Filling
  vaccination_data_interpolated[ which_interpolated_indexes, current_country  ] <- current_country_data_interpolator$y
  
  # Checking the results
  which_available_indexes <- which( vaccination_data_interpolated$date %in% current_country_data$date )
  
  cat(current_country, " control sort -> ",    sum(!which_interpolated_indexes == sort(which_interpolated_indexes)), "\n")
  cat(current_country, " control vaccine -> ", sum(!vaccination_data_interpolated[ which_interpolated_indexes, current_country  ] == current_country_data_interpolator$y), "\n")
  cat(current_country, " control date -> ",    sum(!vaccination_data_interpolated[ which_interpolated_indexes, "date"  ] == current_country_data_interpolator$x), "\n\n")
  cat(current_country, " control match 1 -> ", sum(!vaccination_data_interpolated[ which_available_indexes, current_country ] == current_country_data$value), "\n\n")
  cat(current_country, " control match 2 -> ", sum(!vaccination_data_interpolated[ which_available_indexes, "date" ] == current_country_data$date), "\n\n")
  
  # for ( current_country in travel_meta_countries$country )    
}  


# Saving the data
save(vaccination_data_interpolated, file = paste("../R_Data/vaccination_data_interpolated.RData"))



# Fix 2023.06.03.
# Creating daily counts versions

# Creating a frame
vaccination_data_selected_fixed <- data.frame( date = date_formatted, dat_matrix_raw )
names(vaccination_data_selected_fixed)[-1] <- sort(travel_meta_countries$country)
row_count <- dim(dat_matrix_raw)[1]


for (country in countries)
{
  # Debugging
  # country <- countries[1]
  # cat(country, "\n")
  
  temp_diff = diff(vaccination_data_interpolated[,country])
  
  # Extra checks
  cat(country, "sum( temp_diff < 0 ) -> \t", sum( temp_diff < 0 ), "\n\n")
  vaccination_data_selected_fixed[,country] = c(0, temp_diff)

# End of -> for (country in countries)
}
# Saving the processed results as RData file.
save(vaccination_data_selected_fixed, file = paste("../R_Data/vaccination_data_selected_fixed.RData"))










# Fix 2023.06.03.
# Aggregating data.

# Dates range
dates_aggregation <- sort(seq(from = max(vaccination_data_all$date), min(vaccination_data_all$date)-aggregation_periods_in_days, by = - aggregation_periods_in_days))

# Determining columns
dat_matrix_raw_aggregation <- matrix( data = 0, nrow = length(dates_aggregation), ncol = length(travel_meta_countries$country)  )
dim(dat_matrix_raw_aggregation)


# Fix 2023.06.02. 
# Creating a frame for aggregation
vaccination_data_aggregated <- data.frame(date = as.Date(dates_aggregation), dat_matrix_raw_aggregation )
names(vaccination_data_aggregated)[-1] <- sort(travel_meta_countries$country)


# Aggregation loop
for (country_current in countries)
{
  # Debugging
  # country_current <- countries[1]
  
  for (date_current in dates_aggregation)
  {
    # date_current <- dates_aggregation[1]
    # date_current <- dates_aggregation[2]
    # date_current <- dates_aggregation[length(dates_aggregation)]
    
    # vaccination
    # Indexes extraction
    which_dates_current_vaccination <- vaccination_data_selected_fixed$date %in% c((date_current-aggregation_periods_in_days + 1):date_current)
    # Saving the sum
    vaccination_data_aggregated[(vaccination_data_aggregated$date == date_current),country_current] <- 
      sum(vaccination_data_selected_fixed[ which_dates_current_vaccination, country_current]) 
    
    # End of -> for (current_country in countries)   
  }  
  
  
  # End of -> for (current_country in countries)
}  
save(vaccination_data_aggregated, file = paste("../R_Data/vaccination_data_aggregated.RData"))










# Creating a frame
vaccination_data_frame_standardized <- data.frame(date = as.Date(dates_aggregation), dat_matrix_raw_aggregation )
names(vaccination_data_frame_standardized)[-1] <- sort(travel_meta_countries$country)


# Filling the frame!
for ( current_country in travel_meta_countries$country )
{
  # Debuging
  # current_country <- travel_meta_countries$country[1]
  # current_country <- travel_meta_countries$country[2]
  
  # Extracting the data for the given country
  current_country_data       <- vaccination_data_aggregated[ , current_country]
  current_country_population <- population_data_selected[ population_data_selected$Country == current_country , "Counts2020"]
  
  # Standardizing data
  vaccination_data_frame_standardized[, current_country] <- rates_per_counts * current_country_data / current_country_population

  
# for ( current_country in travel_meta_countries$country )    
}  




# Saving the data
save(vaccination_data_frame_standardized, file = paste("../R_Data/vaccination_data_frame_standardized.RData"))







