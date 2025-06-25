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
part12_01_inteval_aggregations_path <- paste0("../R_Data/part12_01_inteval_aggregations.RData") 
# Loading RData
load( file = part12_01_inteval_aggregations_path )



# Reading data
# Incidence from RData file.
# load(file = paste("../R_Data/incidence_data_standardized.RData"))
# Mortality as RData file.
# load(file = paste("../R_Data/mortality_data_standardized.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]





# Printing summaries
# tail(incidence_data_selected_fixed_plus_standardized)
# tail(mortality_data_selected_fixed_plus_standardized)

# For entire Europe extension
graph_suffix = "_eu"

# Creating a frame of pairs
# pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)



# incidence
load(file = paste("../R_Data/pairs_frame_incidence_tw.RData"))
# mortality
load(file = paste("../R_Data/pairs_frame_mortality_tw.RData"))
ls()





# Fix 2025.06.23.
# Current Aggregation step
for( aggr_current_index in c(1:length(part12_01_inteval_aggregations)) )
{
  # Debugging
  # aggr_current_index <- 1 
  
  print("Current Aggregation:")
  print(aggr_current_index)
  

  # Creating matrices of distances
  # Extracting dimensions
  matrix_dim = length(countries)
  

  # incidence
  tw_distance_matrix_incidence_c37 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
  colnames(tw_distance_matrix_incidence_c37) = countries
  rownames(tw_distance_matrix_incidence_c37) = countries
  
  # Filling the matrix
  for (current_country1 in countries)
  {
    for (current_country2 in countries)
    {
      # Debugging step
      # current_country1 = countries[length(countries)]
      # current_country2 = countries[2]
      
      # current_country1 = countries[35]
      # current_country2 = countries[15]
      
      
      # Extracting row index
      row_index = which((pairs_frame_incidence_tw$Country1 == current_country1) * (pairs_frame_incidence_tw$Country2 == current_country2) +
                          (pairs_frame_incidence_tw$Country1 == current_country2) * (pairs_frame_incidence_tw$Country2 == current_country1) == 1)
      
      # Filling the value
      # Extracting the correct tw_statistic
      command_to_extract <- paste0("tw_statistic_incidence_current_aggr <- pairs_frame_incidence_tw$tw_statistic_len", aggr_current_index ) 
      eval(parse(text = command_to_extract))    
      
      if (current_country1 != current_country2) {
        tw_distance_matrix_incidence_c37[current_country1, current_country2] = tw_statistic_incidence_current_aggr[row_index]
      }
      
      
      # End of -> for ( current_country2 in countries )
    }
    
    # End of -> for ( current_country1 in countries )
  }
  
  
  # mortality
  tw_distance_matrix_mortality_c37 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
  colnames(tw_distance_matrix_mortality_c37) = countries
  rownames(tw_distance_matrix_mortality_c37) = countries
  # Filling the matrix
  for (current_country1 in countries)
  {
    for (current_country2 in countries)
    {
      # Debugging step
      # current_country1 = countries[length(countries)]
      # current_country2 = countries[2]
      
      # current_country1 = countries[35]
      # current_country2 = countries[15]
      
      
      # Extracting row index
      row_index = which((pairs_frame_mortality_tw$Country1 == current_country1) * (pairs_frame_mortality_tw$Country2 == current_country2) +
                          (pairs_frame_mortality_tw$Country1 == current_country2) * (pairs_frame_mortality_tw$Country2 == current_country1) == 1)
      
      # Filling the value
      # Extracting the correct tw_statistic
      command_to_extract <- paste0("tw_statistic_mortality_current_aggr <- pairs_frame_mortality_tw$tw_statistic_len", aggr_current_index ) 
      eval(parse(text = command_to_extract))    
      
      if (current_country1 != current_country2) {
        tw_distance_matrix_mortality_c37[current_country1, current_country2] = tw_statistic_mortality_current_aggr[row_index]
      }
      
      
      # End of -> for ( current_country2 in countries )
    }
    
    # End of -> for ( current_country1 in countries )
  }
  
  

  # Fix 2022.12.05.
  # Standardizing distances
  # incidence
  tw_distance_matrix_incidence_c37 <- tw_distance_matrix_incidence_c37/max(tw_distance_matrix_incidence_c37) * 1000

  # Fix 2022.12.05.
  # Standardizing distances
  # mortality
  tw_distance_matrix_mortality_c37 <- tw_distance_matrix_mortality_c37/max(tw_distance_matrix_mortality_c37) * 1000


  # Function for hclustering  
  hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
    cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
    hclust(as.dist(x), method = method_selected)
  }
  
  
  # incidence
  
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    # Computing clusters
    cluster_incidence = hclustfunc(tw_distance_matrix_incidence_c37, method_selected = method_current)
    
    
    # Fix 2022.11.28 
    
    # Creating object
    create_object_command <- paste0( "dynamic_time_wapping_hclust_incidence_c37_", method_current, "_aggr_", aggr_current_index, " <- cluster_incidence" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/dynamic_time_wapping_hclust_incidence_c37_", method_current, "_aggr_", aggr_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( dynamic_time_wapping_hclust_incidence_c37_", method_current, "_aggr_", aggr_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    
    

  # End of -> for (method_current in hclust_methods)  
  }
  
  
  
  
  
  # mortality
  
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    # Computing clusters
    cluster_mortality = hclustfunc(tw_distance_matrix_mortality_c37, method_selected = method_current)
    
    
    # Fix 2022.11.28 
    
    # Creating object
    create_object_command <- paste0( "dynamic_time_wapping_hclust_mortality_c37_", method_current, "_aggr_", aggr_current_index, " <- cluster_mortality" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/dynamic_time_wapping_hclust_mortality_c37_", method_current, "_aggr_", aggr_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( dynamic_time_wapping_hclust_mortality_c37_", method_current, "_aggr_", aggr_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    

    
    # End of -> for (method_current in hclust_methods)  
  }
  
  
  
  
  
# End of -> for( aggr_current_index in c(1:dim(part12_01_inteval_aggregations)[1]) )
}  
  






