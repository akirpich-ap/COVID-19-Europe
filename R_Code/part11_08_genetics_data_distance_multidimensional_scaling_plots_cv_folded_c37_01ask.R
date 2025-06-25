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
# genetics from RData file.
load(file = paste("../R_Data/genetics_data_matrix_ordered_c37.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]

# Creating complete copy
genetics_data_matrix_ordered_c37_all_countries <- genetics_data_matrix_ordered_c37





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

  
  # Extracting the proper indexes
  # Vaccination
  genetics_data_matrix_ordered_c37 <- genetics_data_matrix_ordered_c37_all_countries[indexes_current_cv, indexes_current_cv]
  dim(genetics_data_matrix_ordered_c37)

  
    
  # Fix 2022.12.05.
  # Standardizing distances
  # genetics
  genetics_data_matrix_ordered_c37 <- genetics_data_matrix_ordered_c37/max(genetics_data_matrix_ordered_c37) * 1000

  
  # Function for hclustering  
  hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
    cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
    hclust(as.dist(x), method = method_selected)
  }
  
  
  # genetics
  
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    # Computing clusters
    cluster_genetics = hclustfunc(genetics_data_matrix_ordered_c37, method_selected = method_current)
    
    
    # Fix 2022.11.28 
    
    # Creating object
    create_object_command <- paste0( "strain_distrubution_distance_hclust_genetics_c37_", method_current, "_fold_", cv_current_index, " <- cluster_genetics" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/strain_distrubution_distance_hclust_genetics_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( strain_distrubution_distance_hclust_genetics_c37_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    

    
    
    
  # End of -> for (method_current in hclust_methods)  
  }
  

  

  
  
# End of -> for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
}  
  






