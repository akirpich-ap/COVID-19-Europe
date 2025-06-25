# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.30. ask
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



# Reading data
# vaccination from RData file.
load(file = paste("../R_Data/vaccination_data_frame_standardized.RData"))
# Countries data
load(file = paste("../R_Data/countries.RData"))
ls()

# Printing summaries
tail(vaccination_data_frame_standardized)


# Creating a frame of pairs
pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)




# vaccination

# Creating a frame with results.
pairs_frame_vaccination_cc = data.frame(pairs_frame, cc_statistic = rep(0, dim(pairs_frame)[1]))
names(pairs_frame_vaccination_cc)[c(1, 2)] = c("Country1", "Country2")

for (current_index in c(1:dim(pairs_frame_vaccination_cc)[1])) {

  # Debugging step
  # current_index = 1 

  # Extracting country names for the current pair.
  country1 = pairs_frame_vaccination_cc$Country1[current_index]
  country2 = pairs_frame_vaccination_cc$Country2[current_index]

  # Extracting time series
  country1_data = vaccination_data_frame_standardized[,country1] * 1000000
  country2_data = vaccination_data_frame_standardized[,country2] * 1000000

  # Saving 1 - the largest cc function  
  ts_test_current_result <- 1 - max( ccf(x = country1_data, y = country2_data, plot = FALSE)$acf )
  

  # Saving the results into the frame
  pairs_frame_vaccination_cc$cc_statistic[current_index] <- ts_test_current_result
  
  # Debugging step
  cat("current_index = ", current_index, "out of = ", dim(pairs_frame_vaccination_cc)[1], "country1 = ", country1, " vs country2 = ", country2, "\n")
  
  
  # End of -> for ( current_index in c(1:dim(pairs_frame_vaccination_cc)[1]) )
}





pairs_frame_vaccination_cc



# Saving the processed results as RData files.
# vaccination
save(pairs_frame_vaccination_cc, file = paste("../R_Data/pairs_frame_vaccination_cc.RData"))



# Saving the processed results as LaTeX files.
# Creating object of type xtable from library(xtable) from the ones what we read. 
pairs_frame_vaccination_cc_xtable = xtable(pairs_frame_vaccination_cc, digits = 20)



# Exporting the xtable results with the same filename ad R frame names but with the different extension (tex).
# vaccination
pairs_frame_vaccination_cc_xtable_path = paste("../R_Output/pairs_frame_vaccination_cc_xtable.tex", sep = "")
print.xtable(pairs_frame_vaccination_cc_xtable, type = "latex", file = pairs_frame_vaccination_cc_xtable_path, include.rownames = FALSE)

