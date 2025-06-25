# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.06.23. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen = 20)


# Library to perform column medians and other useful matrix algebra computations.
# library(matrixStats)

# Library for the latex exports in the nice format.
# library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
# library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
# library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
# library(readr)

# To convert dates from text to dates
# library(lubridate)

# Library for permutations
library(gtools)

# This is to use the capitalized names i.e. for str_to_title() 
library(stringr)

# This package is used to produce latex tables.
# library(xtable)


# Loading the cv setup
# Path
part11_01_cross_validation_samples_to_keep_path <- paste0("../R_Data/part11_01_cross_validation_samples_to_keep.RData") 
# Loading RData
load( file = part11_01_cross_validation_samples_to_keep_path )




# Reading data
load(file = paste("../R_Data/countries_travel.RData"))
countries_travel_all_countires <- countries_travel
ls()

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")



# For entire Europe extension
graph_suffix = "_eu"

# Creating a frame of pairs
# pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)



# Loading Dynamic Time Wrapping statistics
load(file = paste("../R_Data/pairs_frame_travel.RData"))
ls()







# Fix 2025.06.23.
# Cross-validation step
for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
{
  # Debugging
  # cv_current_index <- 1 
  print("Current Fold:")
  print(cv_current_index)
  
  # Current indexes
  indexes_current_cv <- sort(part11_01_cross_validation_samples_to_keep[cv_current_index,])

  # Extracting countries for the given fold
  countries_travel <- countries_travel_all_countires[indexes_current_cv]
  
  
  # Creating matrices of distances
  # Extracting dimensions
  matrix_dim <- length(countries_travel)
  
  
  # travel_statistic_average_2011_2016
  matrix_distance_travel_statistic_average_2011_2016 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
  colnames(matrix_distance_travel_statistic_average_2011_2016) = countries_travel
  rownames(matrix_distance_travel_statistic_average_2011_2016) = countries_travel
  
  # Getting minimum summaries 
  values_duplicated_statistic_average_2011_2016 <- pairs_frame_travel$travel_statistic_average_2011_2016
  values_duplicated_statistic_average_2011_2016 <-  values_duplicated_statistic_average_2011_2016 / max(values_duplicated_statistic_average_2011_2016)
  
  min_values_duplicated_statistic_average_2011_2016    <-  min(values_duplicated_statistic_average_2011_2016)
  max_values_duplicated_statistic_average_2011_2016    <-  max(values_duplicated_statistic_average_2011_2016)
  mean_values_duplicated_statistic_average_2011_2016   <-  mean(values_duplicated_statistic_average_2011_2016)
  
  
  # Filling the matrix
  for (current_country1 in countries_travel)
  {
    for (current_country2 in countries_travel)
    {
      # Debugging step
      # current_country1 = countries[length(countries)]
      # current_country2 = countries[2]
      
      # current_country1 = countries_travel[35]
      # current_country2 = countries_travel[15]
      
      
      # Extracting row index
      row_index = which((pairs_frame_travel$Country1 == current_country1) * (pairs_frame_travel$Country2 == current_country2) +
                          (pairs_frame_travel$Country1 == current_country2) * (pairs_frame_travel$Country2 == current_country1) == 1)
      
      # Filling the value
      if (current_country1 != current_country2) {
        matrix_distance_travel_statistic_average_2011_2016[current_country1, current_country2] = 
          # (max(pairs_frame_travel$travel_statistic_average_2011_2016) - pairs_frame_travel$travel_statistic_average_2011_2016[row_index])/median(pairs_frame_travel$travel_statistic_average_2011_2016)
          # max(pairs_frame_travel$travel_statistic_average_2011_2016) - pairs_frame_travel$travel_statistic_average_2011_2016[row_index]
          exp(- (values_duplicated_statistic_average_2011_2016[row_index] - min_values_duplicated_statistic_average_2011_2016)/mean_values_duplicated_statistic_average_2011_2016  )
        
      }
      
      
      # End of -> for ( current_country2 in countries )
    }
    
    # End of -> for ( current_country1 in countries )
  }
  

  # Fix 2022.12.05.
  # Standardizing distances
  # incidence
  matrix_distance_travel_statistic_average_2011_2016 <- matrix_distance_travel_statistic_average_2011_2016/max(matrix_distance_travel_statistic_average_2011_2016) * 1000

  

  # Function for hclustering  
  hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
    cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
    hclust(as.dist(x), method = method_selected)
  }
  
  
  # travel_statistic_average_2011_2016
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    # Computing clusters
    cluster_travel_statistic_average_2011_2016 = hclustfunc(matrix_distance_travel_statistic_average_2011_2016, method_selected = method_current)
    
    
    # Fix 2022.11.28 
    
    # Creating object
    create_object_command <- paste0( "travel_matrix_hclust_average_2011_2016_", method_current, "_fold_", cv_current_index, " <- cluster_travel_statistic_average_2011_2016" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/travel_matrix_hclust_average_2011_2016_", method_current, "_fold_", cv_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( travel_matrix_hclust_average_2011_2016_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    
    
    

  # End of ->  for (method_current in hclust_methods)         
  } 
     

  
# End of -> for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
}  
  






