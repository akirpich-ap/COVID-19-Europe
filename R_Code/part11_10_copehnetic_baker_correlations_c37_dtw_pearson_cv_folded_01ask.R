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

# Library to work with dendrograms
library(dendextend)


# This is to use the capitalized names i.e. for str_to_title() 
library(stringr)

# This package is used to produce latex tables.
# library(xtable)

# to extract unique rows in data.frame
library(dplyr)


# Loading the cv setup
# Path
part11_01_cross_validation_samples_to_keep_path <- paste0("../R_Data/part11_01_cross_validation_samples_to_keep.RData") 
# Loading RData
load( file = part11_01_cross_validation_samples_to_keep_path )




# Reading data
load(file = paste("../R_Data/countries.RData"))
ls()

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]



vector_of_names <- c("auxilliary_data_hclust_complete_c37_dengrogram_current",
                     "auxilliary_data_hclust_demographics_c37_dengrogram_current",
                     "auxilliary_data_hclust_population_health_c37_dengrogram_current",
                     "dynamic_time_wapping_hclust_incidence_c37_dengrogram_current",
                     "dynamic_time_wapping_hclust_mortality_c37_dengrogram_current",
                     "dynamic_time_wapping_hclust_vaccination_c37_dengrogram_current",
                     "strain_distrubution_distance_hclust_genetics_c37_dengrogram_current",
                     "travel_matrix_hclust_average_2011_2016_dengrogram_current" )           


vector_of_titles <- c("Population Health and Socio-Demographic Data",
                      "Socio-Demographic Data",
                      "Population Health Data",
                      "COVID-19 Incidence (Dynamic Time Wrapping Distance)",
                      "COVID-19 Mortality (Dynamic Time Wrapping Distance)",
                      "COVID-19 Vaccinations (Dynamic Time Wrapping Distance)",
                      "COVID-19 Genetic Diversity (Strain Distribution Distance)",
                      "Mobility Data Average for 2011-2016")

# Putting data into a frame
vectors_frame <- data.frame(name = vector_of_names, title = vector_of_titles)
vectors_frame


# Fix 2025.06.23.
# Cross-validation step
# Working with the current fold.
for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
{
  # Debugging
  # cv_current_index <- 1 
  print("Current Fold:")
  print(cv_current_index)
  
  # Current indexes
  indexes_current_cv <- sort(part11_01_cross_validation_samples_to_keep[cv_current_index,])

  
  
  # Working with the current method within the current fold.
  
  for (method_current in hclust_methods)
  {
    # Debugging step 
    # method_current = hclust_methods[1]
    
    
    # dynamic_time_wapping_hclust
    # path
    dynamic_time_wapping_hclust_incidence_path <- paste0("../R_Data/dynamic_time_wapping_hclust_incidence_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    dynamic_time_wapping_hclust_mortality_path <- paste0("../R_Data/dynamic_time_wapping_hclust_mortality_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = dynamic_time_wapping_hclust_incidence_path )
    load( file = dynamic_time_wapping_hclust_mortality_path )

    
    # dynamic_time_wapping_hclust
    # path
    dynamic_time_wapping_hclust_incidence_path <- paste0("../R_Data/dynamic_time_wapping_hclust_incidence_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    dynamic_time_wapping_hclust_mortality_path <- paste0("../R_Data/dynamic_time_wapping_hclust_mortality_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = dynamic_time_wapping_hclust_incidence_path )
    load( file = dynamic_time_wapping_hclust_mortality_path )
    
    
    # travel_matrix_hclust
    # path
    travel_matrix_hclust_average_2011_2016_path <- paste0("../R_Data/travel_matrix_hclust_average_2011_2016_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = travel_matrix_hclust_average_2011_2016_path )
    
    
    # auxilliary_data_hclust_complete
    # path
    auxilliary_data_hclust_complete_path          <- paste0("../R_Data/auxilliary_data_hclust_complete_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    auxilliary_data_hclust_demographics_path      <- paste0("../R_Data/auxilliary_data_hclust_demographics_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    auxilliary_data_hclust_population_health_path <- paste0("../R_Data/auxilliary_data_hclust_population_health_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = auxilliary_data_hclust_complete_path )
    load( file = auxilliary_data_hclust_demographics_path )
    load( file = auxilliary_data_hclust_population_health_path )
    
    
    # dynamic_time_wapping_hclust
    # path
    dynamic_time_wapping_hclust_vaccination_path <- paste0("../R_Data/dynamic_time_wapping_hclust_vaccination_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = dynamic_time_wapping_hclust_vaccination_path )
    
    
    # dynamic_time_wapping_hclust
    # path
    dynamic_time_wapping_hclust_vaccination_path <- paste0("../R_Data/dynamic_time_wapping_hclust_vaccination_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = dynamic_time_wapping_hclust_vaccination_path )
    
    
    # strain_distrubution_distance_hclust
    # path
    strain_distrubution_distance_hclust_genetics_path <- paste0("../R_Data/strain_distrubution_distance_hclust_genetics_c37_", method_current, "_fold_", cv_current_index, ".RData") 
    # loading
    load( file = strain_distrubution_distance_hclust_genetics_path )
    
    ls()
    
    
    
    # Generating Dendrograms for the current method
    
    # dynamic_time_wapping_hclust
    # text
    dynamic_time_wapping_hclust_incidence_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_incidence_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_incidence_c37_", method_current, "_fold_", cv_current_index, ")") 
    dynamic_time_wapping_hclust_mortality_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_mortality_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_mortality_c37_", method_current, "_fold_", cv_current_index, ")") 
    # running 
    eval(parse(text = dynamic_time_wapping_hclust_incidence_dengrogram_text))    
    eval(parse(text = dynamic_time_wapping_hclust_mortality_dengrogram_text))    
    
    
    # dynamic_time_wapping_hclust
    # text
    dynamic_time_wapping_hclust_incidence_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_incidence_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_incidence_c37_", method_current, "_fold_", cv_current_index, ")") 
    dynamic_time_wapping_hclust_mortality_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_mortality_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_mortality_c37_", method_current, "_fold_", cv_current_index, ")") 
    # running 
    eval(parse(text = dynamic_time_wapping_hclust_incidence_dengrogram_text))    
    eval(parse(text = dynamic_time_wapping_hclust_mortality_dengrogram_text))    
    
    
    # travel_matrix_hclust
    # text
    travel_matrix_hclust_average_2011_2016_dengrogram_text   <- paste0("travel_matrix_hclust_average_2011_2016_dengrogram_current <- as.dendrogram(travel_matrix_hclust_average_2011_2016_", method_current, "_fold_", cv_current_index, ")") 
    # run
    eval(parse(text = travel_matrix_hclust_average_2011_2016_dengrogram_text)) 
    
    
    # auxilliary_data_hclust_complete
    # text
    auxilliary_data_hclust_complete_dengrogram_text          <- 
      paste0("auxilliary_data_hclust_complete_c37_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_complete_c37_", method_current, "_fold_", cv_current_index, ")")
    
    auxilliary_data_hclust_demographics_dengrogram_text      <- 
      paste0("auxilliary_data_hclust_demographics_c37_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_demographics_c37_", method_current, "_fold_", cv_current_index, ")") 
    
    auxilliary_data_hclust_population_health_dengrogram_text <- 
      paste0("auxilliary_data_hclust_population_health_c37_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_population_health_c37_", method_current, "_fold_", cv_current_index, ")") 
    # run
    eval(parse(text = auxilliary_data_hclust_complete_dengrogram_text))    
    eval(parse(text = auxilliary_data_hclust_demographics_dengrogram_text))    
    eval(parse(text = auxilliary_data_hclust_population_health_dengrogram_text))    
    
    
    # dynamic_time_wapping_hclust
    # text
    dynamic_time_wapping_hclust_vaccination_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_vaccination_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_vaccination_c37_", method_current, "_fold_", cv_current_index, ")") 
    # running 
    eval(parse(text = dynamic_time_wapping_hclust_vaccination_dengrogram_text))    

    
    # dynamic_time_wapping_hclust
    # text
    dynamic_time_wapping_hclust_vaccination_dengrogram_text <- 
      paste0("dynamic_time_wapping_hclust_vaccination_c37_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_vaccination_c37_", method_current, "_fold_", cv_current_index, ")") 
    # running 
    eval(parse(text = dynamic_time_wapping_hclust_vaccination_dengrogram_text))    
    
    
    # strain_distrubution_distance_hclust
    # text
    strain_distrubution_distance_hclust_genetics_dengrogram_text <- 
      paste0("strain_distrubution_distance_hclust_genetics_c37_dengrogram_current <- as.dendrogram(strain_distrubution_distance_hclust_genetics_c37_", method_current, "_fold_", cv_current_index, ")") 
    # running 
    eval(parse(text = strain_distrubution_distance_hclust_genetics_dengrogram_text))    
    
    
    
    
    
    
    # Creating a list of dendrograms
    dendlist_text_raw <- paste0(vectors_frame$name, collapse = ", ") 
    dendlist_text <- paste0("dendlist_of_dendrograms <- dendlist(", dendlist_text_raw ,")") 
    # running 
    eval(parse(text = dendlist_text))    

    # Fixing the names for the current list
    names(dendlist_of_dendrograms) <- vectors_frame$title
    
    
    # list of correlations
    correlations_list = c("cophenetic", "baker")
    
    
    # Looping to save over correlations
    for ( correlation_current in c(1:length(correlations_list)) )
    {
      # Debugging 
      # correlation_current <-  1
      
      # Computing current correlation
      cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = correlations_list[correlation_current] )
      

      # Saving object
      object_creation_for_current_correlation_and_fold_code <- paste0("dtw_correlation_", correlations_list[correlation_current] , "_", method_current, "_fold_", cv_current_index, " <- cor_dendlist_current") 
      eval(parse(text = object_creation_for_current_correlation_and_fold_code))    
      
      # Path to save
      path_to_save_object <- paste0("../R_Data/dtw_correlation_", correlations_list[correlation_current] , "_", method_current, "_fold_", cv_current_index, ".RData") 
      # Saving process
      saving_process_command <- paste0( "save( dtw_correlation_", correlations_list[correlation_current] , "_", method_current, "_fold_", cv_current_index, ", file = path_to_save_object )" )
      eval(parse(text = saving_process_command))    

            
      # End of -> for ( correlation_current in c(1:length(correlations_list)) )  
    }  
    
    
    
    
    
    
  # End of -> for (method_current in hclust_methods)
  } 
  
  
 # Listing available objects
 # ls()

  
# End of -> for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
}  
  






