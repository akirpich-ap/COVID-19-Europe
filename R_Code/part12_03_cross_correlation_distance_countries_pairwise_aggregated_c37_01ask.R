# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.06.23. ask
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

# Library for permutations
library(gtools)

# This package is used to produce latex tables.
library(xtable)


# This package is used for dynamic time wrapping (DTW).
library(dtw)



# Reading data
# Incidence from RData file.
load(file = paste("../R_Data/incidence_data_standardized.RData"))
# Mortality as RData file.
load(file = paste("../R_Data/mortality_data_standardized.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()


# Loading the cv setup
# Path
part12_01_inteval_aggregations_path <- paste0("../R_Data/part12_01_inteval_aggregations.RData") 
# Loading RData
load( file = part12_01_inteval_aggregations_path )


# Printing summaries
tail(incidence_data_selected_fixed_plus_standardized)
tail(mortality_data_selected_fixed_plus_standardized)

# Creating a frame of pairs
pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)



# Function to aggregate by n elements
# If the last piece it shorter it will also be kept
sum_by_n_keep_remaining <- function(vec, n) 
{
  result <- c()
  len <- length(vec)
  
  for (i in seq(1, len, by = n))
  {
    chunk <- vec[i:min(i + n - 1, len)]
    result <- c(result, sum(chunk))
  }
  
  return(result)
}


# incidence
# Creating a frame with results.
pairs_frame_incidence_cc = data.frame(pairs_frame, cc_statistic = rep(0, dim(pairs_frame)[1]))
names(pairs_frame_incidence_cc)[c(1, 2)] = c("Country1", "Country2")


# mortality
# Creating a frame with results.
pairs_frame_mortality_cc = data.frame(pairs_frame, cc_statistic = rep(0, dim(pairs_frame)[1]))
names(pairs_frame_mortality_cc)[c(1, 2)] = c("Country1", "Country2")



for( len_current in part12_01_inteval_aggregations )
{
  # Debugging
  # len_current <-  part12_01_inteval_aggregations[1]

  
  # incidence

  for (current_index in c(1:dim(pairs_frame_incidence_cc)[1])) {
    
    # Debugging step
    # current_index = 1 
    
    # Extracting country names for the current pair.
    country1 = pairs_frame_incidence_cc$Country1[current_index]
    country2 = pairs_frame_incidence_cc$Country2[current_index]
    
    # Extracting time series
    # Aggregation by len_current
    country1_data = sum_by_n_keep_remaining(vec = incidence_data_selected_fixed_plus_standardized[,country1], n = len_current ) * 1000000
    country2_data = sum_by_n_keep_remaining(vec = incidence_data_selected_fixed_plus_standardized[,country2], n = len_current ) * 1000000
    
    
    # Saving 1 - the largest cc function  
    ts_test_current_result <- 1 - max( ccf(x = country1_data, y = country2_data, plot = FALSE)$acf )
    
    
    # Saving the results into the frame
    if (len_current == 1)
    {
      pairs_frame_incidence_cc$cc_statistic[current_index] <- ts_test_current_result
    }  


    filling_cc_current <- paste0( "pairs_frame_incidence_cc$cc_statistic_len", len_current, "[current_index] <- ts_test_current_result" ) 
    eval(parse(text = filling_cc_current))    
    
    
    # Debugging step
    cat("current_index = ", current_index, "out of = ", dim(pairs_frame_incidence_cc)[1], "country1 = ", country1, " vs country2 = ", country2, "\n")
    
    
    # End of -> for ( current_index in c(1:dim(pairs_frame_incidence_cc)[1]) )
  }
  
  
  
  
  # mortality
  
  for (current_index in c(1:dim(pairs_frame_mortality_cc)[1])) {
    # Debugging step
    # current_index = 1 
    
    # Extracting country names for the current pair.
    country1 = pairs_frame_mortality_cc$Country1[current_index]
    country2 = pairs_frame_mortality_cc$Country2[current_index]
    
    # Extracting time series
    country1_data_raw = mortality_data_selected_fixed_plus_standardized[,country1] * 1000000
    country2_data_raw = mortality_data_selected_fixed_plus_standardized[,country2] * 1000000
    
    # Aggregation by len_current
    country1_data = sum_by_n_keep_remaining(vec = mortality_data_selected_fixed_plus_standardized[,country1], n = len_current ) * 1000000
    country2_data = sum_by_n_keep_remaining(vec = mortality_data_selected_fixed_plus_standardized[,country2], n = len_current ) * 1000000
    
    
    # Saving 1 - the largest cc function  
    ts_test_current_result <- 1 - max( ccf(x = country1_data, y = country2_data, plot = FALSE)$acf )
    
    
    # Saving the results into the frame
    if (len_current == 1)
    {
      pairs_frame_mortality_cc$cc_statistic[current_index] <- ts_test_current_result
    }  

    
    filling_cc_current <- paste0( "pairs_frame_mortality_cc$cc_statistic_len", len_current, "[current_index] <- ts_test_current_result" ) 
    eval(parse(text = filling_cc_current))    
    
    
    # Debugging step
    cat("current_index = ", current_index, "out of = ", dim(pairs_frame_mortality_cc)[1], "country1 = ", country1, " vs country2 = ", country2, "\n")
    
    
  # End of -> for ( current_index in c(1:dim(pairs_frame_mortality_cc)[1]) )
  }
  
  
  
  
  pairs_frame_incidence_cc
  pairs_frame_mortality_cc
  
  

  # Saving the processed results as RData files.
  # incidence
  save(pairs_frame_incidence_cc, file = paste("../R_Data/pairs_frame_incidence_cc.RData"))
  # mortality
  save(pairs_frame_mortality_cc, file = paste("../R_Data/pairs_frame_mortality_cc.RData"))
  
  
  
  # Saving the processed results as LaTeX files.
  # Creating object of type xtable from library(xtable) from the ones what we read. 
  pairs_frame_incidence_cc_xtable = xtable(pairs_frame_incidence_cc, digits = 20)
  pairs_frame_mortality_cc_xtable = xtable(pairs_frame_mortality_cc, digits = 20)
  
  # Exporting the xtable results with the same filename ad R frame names but with the different extension (tex).
  # incidence
  pairs_frame_incidence_cc_xtable_path = paste("../R_Output/pairs_frame_incidence_cc_xtable.tex", sep = "")
  print.xtable(pairs_frame_incidence_cc_xtable, type = "latex", file = pairs_frame_incidence_cc_xtable_path, include.rownames = FALSE)
  # mortality
  pairs_frame_mortality_cc_xtable_path = paste("../R_Output/pairs_frame_mortality_cc_xtable.tex", sep = "")
  print.xtable(pairs_frame_mortality_cc_xtable, type = "latex", file = pairs_frame_mortality_cc_xtable_path, include.rownames = FALSE)
  
  
  
  
  
  

# End of -> for(l_current in part12_01_inteval_aggregations )
}  
  




