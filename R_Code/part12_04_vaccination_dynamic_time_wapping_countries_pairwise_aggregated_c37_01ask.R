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


# This package is used for dynamic time wrapping (dtw).
library(dtw)



# Reading data
# Vaccination from RData file.
load(file = paste("../R_Data/vaccination_data_frame_standardized.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()


# Loading the cv setup
# Path
part12_01_inteval_aggregations_path <- paste0("../R_Data/part12_01_inteval_aggregations.RData") 
# Loading RData
load( file = part12_01_inteval_aggregations_path )


# Printing summaries
tail(vaccination_data_frame_standardized)


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


# vaccination
# Creating a frame with results.
pairs_frame_vaccination_tw = data.frame(pairs_frame, tw_statistic = rep(0, dim(pairs_frame)[1]))
names(pairs_frame_vaccination_tw)[c(1, 2)] = c("Country1", "Country2")



for( len_current in part12_01_inteval_aggregations )
{
  # Debugging
  # len_current <-  part12_01_inteval_aggregations[1]

  
  # vaccination

  for (current_index in c(1:dim(pairs_frame_vaccination_tw)[1])) {
    
    # Debugging step
    # current_index = 1 
    
    # Extracting country names for the current pair.
    country1 = pairs_frame_vaccination_tw$Country1[current_index]
    country2 = pairs_frame_vaccination_tw$Country2[current_index]
    
    # Extracting time series
    # Aggregation by len_current
    country1_data = sum_by_n_keep_remaining(vec = vaccination_data_frame_standardized[,country1], n = len_current ) * 1000000
    country2_data = sum_by_n_keep_remaining(vec = vaccination_data_frame_standardized[,country2], n = len_current ) * 1000000
    
    
    # Saving the DTW results  
    ts_test_current_result <- dtw(x = country1_data, y = country2_data, keep = TRUE)
    
    
    # Saving the results into the frame
    if (len_current == 1)
    {
      pairs_frame_vaccination_tw$tw_statistic[current_index] <- ts_test_current_result$distance
    }  


    filling_tw_current <- paste0( "pairs_frame_vaccination_tw$tw_statistic_len", len_current, "[current_index] <- ts_test_current_result$distance" ) 
    eval(parse(text = filling_tw_current))    
    
    
    # Debugging step
    cat("current_index = ", current_index, "out of = ", dim(pairs_frame_vaccination_tw)[1], "country1 = ", country1, " vs country2 = ", country2, "\n")
    
    
    # End of -> for ( current_index in c(1:dim(pairs_frame_vaccination_tw)[1]) )
  }
  
  

  
  pairs_frame_vaccination_tw

  # Saving the processed results as RData files.
  # vaccination
  save(pairs_frame_vaccination_tw, file = paste("../R_Data/pairs_frame_vaccination_tw.RData"))

  
  
  # Saving the processed results as LaTeX files.
  # Creating object of type xtable from library(xtable) from the ones what we read. 
  pairs_frame_vaccination_tw_xtable = xtable(pairs_frame_vaccination_tw, digits = 20)
  
  # Exporting the xtable results with the same filename ad R frame names but with the different extension (tex).
  # vaccination
  pairs_frame_vaccination_tw_xtable_path = paste("../R_Output/pairs_frame_vaccination_tw_xtable.tex", sep = "")
  print.xtable(pairs_frame_vaccination_tw_xtable, type = "latex", file = pairs_frame_vaccination_tw_xtable_path, include.rownames = FALSE)

  
  

# End of -> for(l_current in part12_01_inteval_aggregations )
}  
  




