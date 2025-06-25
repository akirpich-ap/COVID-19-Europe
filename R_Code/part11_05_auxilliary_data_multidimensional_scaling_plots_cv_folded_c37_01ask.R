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

# package for pairwise distances
library(rdist)


# This package is used to produce latex tables.
# library(xtable)


# Loading the cv setup
# Path
part11_01_cross_validation_samples_to_keep_path <- paste0("../R_Data/part11_01_cross_validation_samples_to_keep.RData") 
# Loading RData
load( file = part11_01_cross_validation_samples_to_keep_path )




# Reading data
# Loading
load( file = paste("../R_Data/auxilliary_data_merge.RData"))
# Loading standardized
load( file = paste("../R_Data/auxilliary_data_merge_standardized.RData"))
# list of clustering methods
load( file = paste("../R_Data/countries.RData"))
graph_suffix = "_eu"


# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]


hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]





# Picking columns auxilliary_data_merge_standardized
colnames(auxilliary_data_merge_standardized)

variables_population_health <- 
  c("life_expectancy_2019", "cardiovascular_disease_death_rate_2019", "diabetes_prevalence_2019", "share_of_obese_adults_2016")  
variables_demographics      <- 
  c("total_fertility_rate_2017",  "median_age_2015", "population_density_2018", "gdp_per_capita_2018", "human_development_index_2017", "hospital_beds_per1000_2013", "share_of_extreme_poverty_population_2016")


# countries to keep
which_countries_keep <- which(rownames(auxilliary_data_merge_standardized) %in% countries)


# Computing distances - complete  
distance_matrix_complete_all_countries           <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep,], metric = "euclidean")
colnames(distance_matrix_complete_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_complete_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]


# Computing distances - population_health  
distance_matrix_population_health_all_countries           <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep, variables_population_health], metric = "euclidean")
colnames(distance_matrix_population_health_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_population_health_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]


# Computing distances - demographics  
distance_matrix_demographics_all_countries           <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep, variables_demographics], metric = "euclidean")
colnames(distance_matrix_demographics_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_demographics_all_countries) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]






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
  distance_matrix_complete          <-  distance_matrix_complete_all_countries[indexes_current_cv, indexes_current_cv]
  distance_matrix_population_health <-  distance_matrix_population_health_all_countries[indexes_current_cv, indexes_current_cv]
  distance_matrix_demographics      <-  distance_matrix_demographics_all_countries[indexes_current_cv, indexes_current_cv]

  
  # Standardizing distances
  distance_matrix_complete          <-  distance_matrix_complete/max(distance_matrix_complete) * 1000
  distance_matrix_population_health <-  distance_matrix_population_health/max(distance_matrix_population_health) * 1000
  distance_matrix_demographics      <-  distance_matrix_demographics/max(distance_matrix_demographics) * 1000
  


  # Function for hclustering  
  hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
    cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
    hclust(as.dist(x), method = method_selected)
  }
  
  
  # computing hclustering
  
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    # Computing clusters
    cluster_complete          <- hclustfunc(distance_matrix_complete, method_selected = method_current)
    cluster_population_health <- hclustfunc(distance_matrix_population_health, method_selected = method_current)
    cluster_demographics      <- hclustfunc(distance_matrix_demographics, method_selected = method_current)
    

    # complete
    
    # Creating object
    create_object_command <- paste0( "auxilliary_data_hclust_complete_c37_", method_current, "_fold_", cv_current_index, " <- cluster_complete" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_complete_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( auxilliary_data_hclust_complete_c37_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    


    # population_health
    
    # Creating object
    create_object_command <- paste0( "auxilliary_data_hclust_population_health_c37_", method_current, "_fold_", cv_current_index, " <- cluster_population_health" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_population_health_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( auxilliary_data_hclust_population_health_c37_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    

    
    # demographics
    
    # Creating object
    create_object_command <- paste0( "auxilliary_data_hclust_demographics_c37_", method_current, "_fold_", cv_current_index, " <- cluster_demographics" )
    eval(parse(text = create_object_command))    
    
    # Saving object
    # Path to save
    path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_demographics_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # Saving process
    saving_process_command <- paste0( "save( auxilliary_data_hclust_demographics_c37_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
    eval(parse(text = saving_process_command))    
    
    
    
  # End of -> for (method_current in hclust_methods)  
  }
  
  
  
  
  
# End of -> for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
}  
  






