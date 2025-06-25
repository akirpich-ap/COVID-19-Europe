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

# LaTeX Exports
library(xtable)


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
                     "cross_correlation_distance_hclust_incidence_c37_dengrogram_current",
                     "cross_correlation_distance_hclust_mortality_c37_dengrogram_current",
                     "cross_correlation_distance_hclust_vaccination_c37_dengrogram_current",
                     "strain_distrubution_distance_hclust_genetics_c37_dengrogram_current",
                     "travel_matrix_hclust_average_2011_2016_dengrogram_current" )           


vector_of_titles <- c("Population Health and Socio-Demographic Data",
                      "Socio-Demographic Data",
                      "Population Health Data",
                      "COVID-19 Incidence (Cross-Correlation Distance)",
                      "COVID-19 Mortality (Cross-Correlation Distance)",
                      "COVID-19 Vaccinations (Cross-Correlation Distance)",
                      "COVID-19 Genetic Diversity (Strain Distribution Distance)",
                      "Mobility Data Average for 2011-2016")


# Putting data into a frame
vectors_frame <- data.frame(name = vector_of_names, title = vector_of_titles)
vectors_frame


# list of correlations
correlations_list = c("cophenetic", "baker")


# Working with the current method
for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current <- hclust_methods[1]
  
  
  
  # list of correlations
  correlations_list = c("cophenetic", "baker")
  
  
  # Looping to save over correlations
  for ( correlation_current in c(1:length(correlations_list)) )
  {
    # Debugging 
    # correlation_current <-  1
    
    
    # Creating an empty list to fill
    matrix_list_current <- list()
    
    
    # Working with the current fold.
    for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
    {
      # Debugging
      # cv_current_index <- 1 
      
      # print("Current Fold:")
      # print(cv_current_index)
      
      # Current indexes
      # indexes_current_cv <- sort(part11_01_cross_validation_samples_to_keep[cv_current_index,])
      
      
      
      # Working with the current method within the current fold.
      
      # Path to load
      path_to_load_object <- paste0("../R_Data/ccd_correlation_", correlations_list[correlation_current] , "_", method_current, "_fold_", cv_current_index, ".RData") 
      # Loading process
      load( file = path_to_load_object )
      
      command_to_add_matrix <- paste0("matrix_list_current[[", cv_current_index, "]] <- ccd_correlation_", correlations_list[correlation_current] , "_", method_current, "_fold_", cv_current_index) 
      eval(parse(text = command_to_add_matrix))    
      
      
    # End of -> for( cv_current_index in c(1:dim(part11_01_cross_validation_samples_to_keep)[1]) )
    }  
    

    command_to_save_with_correct_name <- paste0("matrix_list_ccd_correlation_", correlations_list[correlation_current] , "_", method_current, " <- matrix_list_current") 
    eval(parse(text = command_to_save_with_correct_name))    
    

  # End of -> for ( correlation_current in c(1:length(correlations_list)) )  
  }  

  
  
# End of -> for (method_current in hclust_methods)
} 










# Producing summaries

# list of correlations
correlations_list = c("cophenetic", "baker")


# Working with the current method
for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current <- hclust_methods[1]
  
  
  # Looping to save over correlations
  for ( correlation_current in c(1:length(correlations_list)) )
  {
    # Debugging 
    # correlation_current <-  1
    
    
    
    command_to_save_with_working_name <- paste0("matrix_list_current <- matrix_list_ccd_correlation_", correlations_list[correlation_current] , "_", method_current) 
    eval(parse(text = command_to_save_with_working_name))    
    

    
    matrix_array_current <- array( unlist(matrix_list_current), dim = c( nrow(matrix_list_current[[1]]), ncol(matrix_list_current[[1]]), length(matrix_list_current) ) )
    
    # dim(matrix_array_current)
    
    # lapply(matrix_list_current, function(mat) mean(mat))
    
    

    matrix_current_mean <- apply(matrix_array_current, c(1, 2), mean)
    matrix_current_sd <- apply(matrix_array_current, c(1, 2), sd)
    
    matrix_current_p0.000  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.000) )
    matrix_current_p0.025  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.025) )
    matrix_current_p0.250  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.250) )
    matrix_current_p0.500  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.500) )
    matrix_current_p0.750  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.750) )
    matrix_current_p0.975  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 0.975) )
    matrix_current_p1.000  <- apply(matrix_array_current, c(1, 2), function(x) quantile(x, probs = 1.000) )
  
    
    # Putting into a list
    summaries_list_current <- list(matrix_current_mean,
                                   matrix_current_sd,
                                   matrix_current_p0.000,
                                   matrix_current_p0.025,
                                   matrix_current_p0.250,
                                   matrix_current_p0.500,
                                   matrix_current_p0.750,
                                   matrix_current_p0.975,
                                   matrix_current_p1.000)
    
    names(summaries_list_current) <- c("mean", "sd", "p0.000", "p0.025", "p0.250", "p0.500", "p0.750", "p0.975", "p1.000")

    
    # Preparing for exports    
    current_text_summary_output_raw <- paste0( round(matrix_current_p0.500, 2), " (", round(matrix_current_p0.025, 2), ";", round(matrix_current_p0.975, 2), ")"  )
    
    current_text_summary_out <- matrix( data = current_text_summary_output_raw, nrow = dim(matrix_current_p0.500)[1], ncol = dim(matrix_current_p0.500)[1]  )
    
    
    rownames(current_text_summary_out) <- rownames(matrix_list_current[[1]])
    colnames(current_text_summary_out) <- colnames(matrix_list_current[[1]])
    
    frame_text_summary_out <- data.frame(name = rownames(current_text_summary_out), current_text_summary_out, check.names = FALSE )
    # Fix 2025.06.24. Re-arranging rowns
    frame_text_summary_out <- frame_text_summary_out[sort(rownames(current_text_summary_out)), c("name", sort(rownames(current_text_summary_out))) ] 
    
    
    # Exporting CSV files
    cor_dendlist_current_csv_path = paste0("../R_Output/part11_11_ccd_pearson_cv_folded_summaries_c37_", method_current, "_", correlations_list[correlation_current], ".csv")
    write.table(x = frame_text_summary_out, file = cor_dendlist_current_csv_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    
    # Exporting as tex file
    frame_text_summary_out_xtable <- xtable( x = frame_text_summary_out[,-1], digits = 2 )  
    # Exporting as tex file
    # Creating a path 
    frame_text_summary_out_xtable_path <- paste0("../R_Output/part11_11_ccd_pearson_cv_folded_summaries_c37_", method_current, "_", correlations_list[correlation_current], ".tex")
    # Printing
    print.xtable( x = frame_text_summary_out_xtable, type = "latex", file = frame_text_summary_out_xtable_path, include.rownames = TRUE )
    
    
    
    
  # End of -> for ( correlation_current in c(1:length(correlations_list)) )  
  }  
  
  
  
  # End of -> for (method_current in hclust_methods)
} 


























