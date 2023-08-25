# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.28. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

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

# Library for the latex exports in the nice format.
library(xtable)

# Library for read_csv
#library(readr)

# Library to work with dendrograms
library(dendextend)

# to extract unique rows in data.frame
library(dplyr)

# for corrplot function
library(corrplot)

# This is to use the capitalized names i.e. for str_to_title() 
library(stringr)

# Installing igraph
# install.packages("igraph")
library(igraph)

# Reading hlust objexts data in.
# List of methods
# hclust_methods = c("complete", "average")
hclust_methods = c("average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")

# Reading data
load(file = paste("../R_Data/travel_meta_countries.RData"))




# Fix 2023.02.15. ask
# Label function

label_function <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_value
  x <- x[1] + strwidth(txt, cex=4) * 6 / 5
  y <- y[2] - strheight(txt, cex=4) * 6 / 5
  text(x, y, txt, cex = label_cex )
  
  
# End of -> label_function <- function(label_value = "A", label_cex = 4) {
}  




# Loading datasets from previous runs

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  
  # dynamic_time_wapping_hclust
  # path
  dynamic_time_wapping_hclust_incidence_path <- paste0("../R_Data/dynamic_time_wapping_hclust_incidence_c39_", method_current, ".RData") 
  dynamic_time_wapping_hclust_mortality_path <- paste0("../R_Data/dynamic_time_wapping_hclust_mortality_c39_", method_current, ".RData") 
  # loading
  load( file = dynamic_time_wapping_hclust_incidence_path )
  load( file = dynamic_time_wapping_hclust_mortality_path )

  
  # pointwise_absolute_distance_hclust
  # path
  pointwise_absolute_distance_hclust_incidence_path <- paste0("../R_Data/pointwise_absolute_distance_hclust_incidence_c39_", method_current, ".RData") 
  pointwise_absolute_distance_hclust_mortality_path <- paste0("../R_Data/pointwise_absolute_distance_hclust_mortality_c39_", method_current, ".RData") 
  # loading
  load( file = pointwise_absolute_distance_hclust_incidence_path )
  load( file = pointwise_absolute_distance_hclust_mortality_path )
  
  
  # cross_correlation_distance_hclust
  # path
  cross_correlation_distance_hclust_incidence_path <- paste0("../R_Data/cross_correlation_distance_hclust_incidence_c39_", method_current, ".RData") 
  cross_correlation_distance_hclust_mortality_path <- paste0("../R_Data/cross_correlation_distance_hclust_mortality_c39_", method_current, ".RData") 
  # loading
  load( file = cross_correlation_distance_hclust_incidence_path )
  load( file = cross_correlation_distance_hclust_mortality_path )
  
  
  # travel_matrix_hclust
  # path
  travel_matrix_hclust_2016_path              <- paste0("../R_Data/travel_matrix_hclust_2016_", method_current, ".RData") 
  travel_matrix_hclust_average_2011_2016_path <- paste0("../R_Data/travel_matrix_hclust_average_2011_2016_", method_current, ".RData") 
  # loading
  load( file = travel_matrix_hclust_2016_path )
  load( file = travel_matrix_hclust_average_2011_2016_path )
  

  # auxilliary_data_hclust_complete
  # path
  auxilliary_data_hclust_complete_path          <- paste0("../R_Data/auxilliary_data_hclust_complete_c39_", method_current, ".RData") 
  auxilliary_data_hclust_demographics_path      <- paste0("../R_Data/auxilliary_data_hclust_demographics_c39_", method_current, ".RData") 
  auxilliary_data_hclust_population_health_path <- paste0("../R_Data/auxilliary_data_hclust_population_health_c39_", method_current, ".RData") 
  # loading
  load( file = auxilliary_data_hclust_complete_path )
  load( file = auxilliary_data_hclust_demographics_path )
  load( file = auxilliary_data_hclust_population_health_path )
  
  
  # dynamic_time_wapping_hclust
  # path
  dynamic_time_wapping_hclust_vaccination_path <- paste0("../R_Data/dynamic_time_wapping_hclust_vaccination_c39_", method_current, ".RData") 
  # loading
  load( file = dynamic_time_wapping_hclust_vaccination_path )

  
  # pointwise_absolute_distance_hclust
  # path
  pointwise_absolute_distance_hclust_vaccination_path <- paste0("../R_Data/pointwise_absolute_distance_hclust_vaccination_c39_", method_current, ".RData") 
  # loading
  load( file = pointwise_absolute_distance_hclust_vaccination_path )
  
  
  # cross_correlation_distance_hclust
  # path
  cross_correlation_distance_hclust_vaccination_path <- paste0("../R_Data/cross_correlation_distance_hclust_vaccination_c39_", method_current, ".RData") 
  # loading
  load( file = cross_correlation_distance_hclust_vaccination_path )
  
  
  # strain_distrubution_distance_hclust
  # path
  strain_distrubution_distance_hclust_genetics_path <- paste0("../R_Data/strain_distrubution_distance_hclust_genetics_c39_", method_current, ".RData") 
  # loading
  load( file = strain_distrubution_distance_hclust_genetics_path )
  

# End of -> for (method_current in hclust_methods)
} 

# Listing available objects
ls()





# # Standardizing heights to be 1000
# 
# for (method_current in hclust_methods)
# {
#   # Debugging step 
#   # method_current = hclust_methods[1]
#   
#   
#   # dynamic_time_wapping_hclust
#   # text
#   dynamic_time_wapping_hclust_incidence_standartization_text <- 
#     paste0("dynamic_time_wapping_hclust_incidence_c39_", method_current,"$height <- 1000 * dynamic_time_wapping_hclust_incidence_c39_", method_current,"$height/max(dynamic_time_wapping_hclust_incidence_c39_", method_current, "$height)") 
#   dynamic_time_wapping_hclust_mortality_standartization_text <- 
#     paste0("dynamic_time_wapping_hclust_mortality_c39_", method_current,"$height <- 1000 * dynamic_time_wapping_hclust_mortality_c39_", method_current,"$height/max(dynamic_time_wapping_hclust_mortality_c39_", method_current, "$height)") 
#   # running 
#   eval(parse(text = dynamic_time_wapping_hclust_incidence_standartization_text))    
#   eval(parse(text = dynamic_time_wapping_hclust_mortality_standartization_text))    
#   
#   
#   # pointwise_absolute_distance_hclust
#   # text
#   pointwise_absolute_distance_hclust_incidence_standartization_text <- 
#     paste0("pointwise_absolute_distance_hclust_incidence_c39_", method_current,"$height <- 1000 * pointwise_absolute_distance_hclust_incidence_c39_", method_current,"$height/max(pointwise_absolute_distance_hclust_incidence_c39_", method_current, "$height)") 
#   pointwise_absolute_distance_hclust_mortality_standartization_text <- 
#     paste0("pointwise_absolute_distance_hclust_mortality_c39_", method_current,"$height <- 1000 * pointwise_absolute_distance_hclust_mortality_c39_", method_current,"$height/max(pointwise_absolute_distance_hclust_mortality_c39_", method_current, "$height)") 
#   # running 
#   eval(parse(text = pointwise_absolute_distance_hclust_incidence_standartization_text))    
#   eval(parse(text = pointwise_absolute_distance_hclust_mortality_standartization_text))    
#   
#   
#   # travel_matrix_hclust
#   # text
#   travel_matrix_hclust_2016_standartization_text   <- 
#     paste0("travel_matrix_hclust_2016_", method_current,"$height <- 1000 * travel_matrix_hclust_2016_", method_current,"$height/max(travel_matrix_hclust_2016_", method_current, "$height)") 
#   travel_matrix_hclust_average_2011_2016_standartization_text   <- 
#     paste0("travel_matrix_hclust_average_2011_2016_", method_current,"$height <- 1000 * travel_matrix_hclust_average_2011_2016_", method_current,"$height/max(travel_matrix_hclust_average_2011_2016_", method_current, "$height)") 
#   # run
#   eval(parse(text = travel_matrix_hclust_2016_standartization_text))    
#   eval(parse(text = travel_matrix_hclust_average_2011_2016_standartization_text)) 
#   
#   
#   # auxilliary_data_hclust_complete
#   # text
#   auxilliary_data_hclust_complete_standartization_text          <- 
#     paste0("auxilliary_data_hclust_complete_c39_", method_current,"$height <- 1000 * auxilliary_data_hclust_complete_c39_", method_current,"$height/max(auxilliary_data_hclust_complete_c39_", method_current, "$height)") 
#   auxilliary_data_hclust_demographics_standartization_text      <- 
#     paste0("auxilliary_data_hclust_demographics_c39_", method_current,"$height <- 1000 * auxilliary_data_hclust_demographics_c39_", method_current,"$height/max(auxilliary_data_hclust_demographics_c39_", method_current, "$height)") 
#   auxilliary_data_hclust_population_health_standartization_text <- 
#     paste0("auxilliary_data_hclust_population_health_c39_", method_current,"$height <- 1000 * auxilliary_data_hclust_population_health_c39_", method_current,"$height/max(auxilliary_data_hclust_population_health_c39_", method_current, "$height)") 
#   # run
#   eval(parse(text = auxilliary_data_hclust_complete_standartization_text))    
#   eval(parse(text = auxilliary_data_hclust_demographics_standartization_text))    
#   eval(parse(text = auxilliary_data_hclust_population_health_standartization_text))    
#   
#   
#   # dynamic_time_wapping_hclust
#   # text
#   dynamic_time_wapping_hclust_vaccination_standartization_text <- 
#     paste0("dynamic_time_wapping_hclust_vaccination_c39_", method_current,"$height <- 1000 * dynamic_time_wapping_hclust_vaccination_c39_", method_current,"$height/max(dynamic_time_wapping_hclust_vaccination_c39_", method_current, "$height)") 
#   # running 
#   eval(parse(text = dynamic_time_wapping_hclust_vaccination_standartization_text))    
# 
# 
#   # pointwise_absolute_distance_hclust
#   # text
#   pointwise_absolute_distance_hclust_vaccination_standartization_text <- 
#     paste0("pointwise_absolute_distance_hclust_vaccination_c39_", method_current,"$height <- 1000 * pointwise_absolute_distance_hclust_vaccination_c39_", method_current,"$height/max(pointwise_absolute_distance_hclust_vaccination_c39_", method_current, "$height)") 
#   # running 
#   eval(parse(text = pointwise_absolute_distance_hclust_vaccination_standartization_text))    
# 
#   
# 
#   # End of -> for (method_current in hclust_methods)
# } 
# 
# # Listing available objects
# ls()



# Fix 2023.06.14
# Creating plotting table
design_frame <- data.frame( matrix( "", nrow = 10, ncol = 9) )
names(design_frame) <- c("dend1", "title1", "title1_short", "file1", "dend2", "title2", "title2_short", "file2", "same_elements")


# 1 COVID-19 Mortality (Cross-Correlation Distance) vs Population Health Data
design_frame$dend1[1]   <- "cross_correlation_distance_hclust_mortality_c39_dengrogram_current"
design_frame$title1[1]  <- "COVID-19 Mortality (Cross-Correlation Distance)"
design_frame$title1_short[1]  <- "MOR"
design_frame$file1[1]   <- "mortality_ccd"
#
design_frame$dend2[1]   <- "auxilliary_data_hclust_population_health_c39_dengrogram_current"
design_frame$title2[1]  <- "Population Health Data"
design_frame$title2_short[1]  <- "PH"
design_frame$file2[1]   <- "auxilliary_data_population_health"
#
design_frame$same_elements[1]   <- 1


# 2 COVID-19 Mortality (Cross-Correlation Distance) vs COVID-19 Vaccinations (Cross-Correlation Distance)
design_frame$dend1[2]   <- "cross_correlation_distance_hclust_mortality_c39_dengrogram_current"
design_frame$title1[2]  <- "COVID-19 Mortality (Cross-Correlation Distance)"
design_frame$title1_short[2]  <- "MOR"
design_frame$file1[2]   <- "mortality_ccd"
#
design_frame$dend2[2]  <- "cross_correlation_distance_hclust_vaccination_c39_dengrogram_current"
design_frame$title2[2] <- "COVID-19 Vaccinations (Cross-Correlation Distance)"
design_frame$title2_short[2]  <- "VAC"
design_frame$file2[2]  <- "vaccinations_ccd"
#
design_frame$same_elements[2]   <- 1


# 3 COVID-19 Mortality (Cross-Correlation Distance) vs Travel Data 2016
design_frame$dend1[3]   <- "cross_correlation_distance_hclust_mortality_c39_dengrogram_current"
design_frame$title1[3]  <- "COVID-19 Mortality (Cross-Correlation Distance)"
design_frame$title1_short[3]  <- "MOR"
design_frame$file1[3]   <- "mortality_ccd"
#
design_frame$dend2[3]   <- "travel_matrix_hclust_average_2011_2016_dengrogram_current"
design_frame$title2[3]  <- "Mobility Data Average for 2011-2016"
design_frame$title2_short[3]  <- "MOB"
design_frame$file2[3]   <- "travel_mean2011to2016"
#
design_frame$same_elements[3]   <- 0


# 4 COVID-19 Vaccinations (Cross-Correlation Distance) vs Population Health and Socio-Demographic Data
design_frame$dend1[4]  <- "cross_correlation_distance_hclust_vaccination_c39_dengrogram_current"
design_frame$title1[4] <- "COVID-19 Vaccinations (Cross-Correlation Distance)"
design_frame$title1_short[4]  <- "VAC"
design_frame$file1[4]  <- "vaccinations_ccd"
#
design_frame$dend2[4]   <- "auxilliary_data_hclust_demographics_c39_dengrogram_current"
design_frame$title2[4]  <- "Socio-Demographic Data"
design_frame$title2_short[4]  <- "SD"
design_frame$file2[4]   <- "auxilliary_data_demographics"
#
design_frame$same_elements[4]   <- 1


# 5 COVID-19 Vaccinations (Cross-Correlation Distance) vs Population Health and Socio-Demographic Data
design_frame$dend1[5]  <- "cross_correlation_distance_hclust_vaccination_c39_dengrogram_current"
design_frame$title1[5] <- "COVID-19 Vaccinations (Cross-Correlation Distance)"
design_frame$title1_short[5]  <- "VAC"
design_frame$file1[5]  <- "vaccinations_ccd"
#
design_frame$dend2[5]   <- "travel_matrix_hclust_average_2011_2016_dengrogram_current"
design_frame$title2[5]  <- "Mobility Data Average for 2011-2016"
design_frame$title2_short[5]  <- "MOB"
design_frame$file2[5]   <- "travel_mean2011to2016"
#
design_frame$same_elements[5]   <- 0


# 6 Population Health and Socio-Demographic Data vs COVID-19 Incidence (Cross-Correlation Distance)
design_frame$dend1[6]   <- "auxilliary_data_hclust_complete_c39_dengrogram_current"
design_frame$title1[6]  <- "Population Health and Socio-Demographic Data"
design_frame$title1_short[6]  <- "PH-SD"
design_frame$file1[6]   <- "auxilliary_data_complete"
#
design_frame$dend2[6]   <- "cross_correlation_distance_hclust_incidence_c39_dengrogram_current"
design_frame$title2[6]  <- "COVID-19 Incidence (Cross-Correlation Distance)"
design_frame$title2_short[6]  <- "INC"
design_frame$file2[6]   <- "incidence_ccd"
#
design_frame$same_elements[6]   <- 1


# 7 Mobility Data Average for 2011-2016 vs COVID-19 Incidence (Cross-Correlation Distance)
design_frame$dend1[7]   <- "travel_matrix_hclust_average_2011_2016_dengrogram_current"
design_frame$title1[7]  <- "Mobility Data Average for 2011-2016"
design_frame$title1_short[7]  <- "MOB"
design_frame$file1[7]   <- "travel_mean2011to2016"
#
design_frame$dend2[7]   <- "cross_correlation_distance_hclust_incidence_c39_dengrogram_current"
design_frame$title2[7]  <- "COVID-19 Incidence (Cross-Correlation Distance)"
design_frame$title2_short[7]  <- "INC"
design_frame$file2[7]   <- "incidence_ccd"
#
design_frame$same_elements[7]   <- 0


# 8 COVID-19 Vaccinations (Cross-Correlation Distance) vs COVID-19 Mortality (Cross-Correlation Distance)
design_frame$dend1[8]  <- "cross_correlation_distance_hclust_vaccination_c39_dengrogram_current"
design_frame$title1[8] <- "COVID-19 Vaccinations (Cross-Correlation Distance)"
design_frame$title1_short[8]  <- "VAC"
design_frame$file1[8]  <- "vaccinations_ccd"
#
design_frame$dend2[8]   <- "cross_correlation_distance_hclust_mortality_c39_dengrogram_current"
design_frame$title2[8]  <- "COVID-19 Mortality (Cross-Correlation Distance)"
design_frame$title2_short[8]  <- "MOR"
design_frame$file2[8]   <- "mortality_ccd"
#
design_frame$same_elements[8]   <- 1


# 9 COVID-19 Incidence (Cross-Correlation Distance) vs COVID-19 Vaccinations (Cross-Correlation Distance)
design_frame$dend1[9]  <- "cross_correlation_distance_hclust_incidence_c39_dengrogram_current"
design_frame$title1[9] <- "COVID-19 Incidence (Cross-Correlation Distance)"
design_frame$title1_short[9]  <- "INC"
design_frame$file1[9]  <- "incidence_ccd"
#
design_frame$dend2[9]   <- "cross_correlation_distance_hclust_vaccination_c39_dengrogram_current"
design_frame$title2[9]  <- "COVID-19 Vaccinations (Cross-Correlation Distance)"
design_frame$title2_short[9]  <- "VAC"
design_frame$file2[9]   <- "vaccinations_ccd"
#
design_frame$same_elements[9]   <- 1


# 10 COVID-19 Incidence (Cross-Correlation Distance) vs COVID-19 Genetic Diversity (Strain Distribution Distance)
design_frame$dend1[10]  <- "cross_correlation_distance_hclust_incidence_c39_dengrogram_current"
design_frame$title1[10] <- "COVID-19 Incidence (Cross-Correlation Distance)"
design_frame$title1_short[10]  <- "INC"
design_frame$file1[10]  <- "incidence_ccd"
#
design_frame$dend2[10]   <- "strain_distrubution_distance_hclust_genetics_c39_dengrogram_current"
design_frame$title2[10]  <- "COVID-19 Genetic Diversity (Strain Distribution Distance)"
design_frame$title2_short[10]  <- "GEN"
design_frame$file2[10]   <- "genetics_sdd"
#
design_frame$same_elements[10]   <- 1



# Creating frame of matched titles
frame_names <- data.frame( rbind( as.matrix(design_frame[,c("title1", "title1_short") ]), 
                                  as.matrix(design_frame[,c("title2", "title2_short") ])) )
# Unique rows
frame_names_unique <- distinct(frame_names)
# Unique and ordered
frame_names_unique_ordered <- frame_names_unique[ order(frame_names_unique$title1), ]


# Correlation threshold specification
correlation_threshold_list <- c(0.25, 0.30, 0.35, 0.40, 0.45, 0.50) 





# Generating dendrograms from previous runs
# Doing comparisons plots with the incidence and mortality
# Doing Fowlkes-Mallows index plots

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  
  # dynamic_time_wapping_hclust
  # text
  dynamic_time_wapping_hclust_incidence_dengrogram_text <- 
    paste0("dynamic_time_wapping_hclust_incidence_c39_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_incidence_c39_", method_current, ")") 
  dynamic_time_wapping_hclust_mortality_dengrogram_text <- 
    paste0("dynamic_time_wapping_hclust_mortality_c39_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_mortality_c39_", method_current, ")") 
  # running 
  eval(parse(text = dynamic_time_wapping_hclust_incidence_dengrogram_text))    
  eval(parse(text = dynamic_time_wapping_hclust_mortality_dengrogram_text))    
  
  
  # pointwise_absolute_distance_hclust
  # text
  pointwise_absolute_distance_hclust_incidence_dengrogram_text <- 
    paste0("pointwise_absolute_distance_hclust_incidence_c39_dengrogram_current <- as.dendrogram(pointwise_absolute_distance_hclust_incidence_c39_", method_current, ")") 
  pointwise_absolute_distance_hclust_mortality_dengrogram_text <- 
    paste0("pointwise_absolute_distance_hclust_mortality_c39_dengrogram_current <- as.dendrogram(pointwise_absolute_distance_hclust_mortality_c39_", method_current, ")") 
  # running 
  eval(parse(text = pointwise_absolute_distance_hclust_incidence_dengrogram_text))    
  eval(parse(text = pointwise_absolute_distance_hclust_mortality_dengrogram_text))    
  
  
  # cross_correlation_distance_hclust
  # text
  cross_correlation_distance_hclust_incidence_dengrogram_text <- 
    paste0("cross_correlation_distance_hclust_incidence_c39_dengrogram_current <- as.dendrogram(cross_correlation_distance_hclust_incidence_c39_", method_current, ")") 
  cross_correlation_distance_hclust_mortality_dengrogram_text <- 
    paste0("cross_correlation_distance_hclust_mortality_c39_dengrogram_current <- as.dendrogram(cross_correlation_distance_hclust_mortality_c39_", method_current, ")") 
  # running 
  eval(parse(text = cross_correlation_distance_hclust_incidence_dengrogram_text))    
  eval(parse(text = cross_correlation_distance_hclust_mortality_dengrogram_text))    
  
  
  # travel_matrix_hclust
  # text
  travel_matrix_hclust_2016_dengrogram_text   <- paste0("travel_matrix_hclust_2016_dengrogram_current <- as.dendrogram(travel_matrix_hclust_2016_", method_current, ")") 
  travel_matrix_hclust_average_2011_2016_dengrogram_text   <- paste0("travel_matrix_hclust_average_2011_2016_dengrogram_current <- as.dendrogram(travel_matrix_hclust_average_2011_2016_", method_current, ")") 
  # run
  eval(parse(text = travel_matrix_hclust_2016_dengrogram_text))    
  eval(parse(text = travel_matrix_hclust_average_2011_2016_dengrogram_text)) 
  
  
  # auxilliary_data_hclust_complete
  # text
  auxilliary_data_hclust_complete_dengrogram_text          <- paste0("auxilliary_data_hclust_complete_c39_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_complete_c39_", method_current, ")")
  auxilliary_data_hclust_demographics_dengrogram_text      <- paste0("auxilliary_data_hclust_demographics_c39_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_demographics_c39_", method_current, ")") 
  auxilliary_data_hclust_population_health_dengrogram_text <- paste0("auxilliary_data_hclust_population_health_c39_dengrogram_current <- as.dendrogram(auxilliary_data_hclust_population_health_c39_", method_current, ")") 
  # run
  eval(parse(text = auxilliary_data_hclust_complete_dengrogram_text))    
  eval(parse(text = auxilliary_data_hclust_demographics_dengrogram_text))    
  eval(parse(text = auxilliary_data_hclust_population_health_dengrogram_text))    
  
  
  # dynamic_time_wapping_hclust
  # text
  dynamic_time_wapping_hclust_vaccination_dengrogram_text <- paste0("dynamic_time_wapping_hclust_vaccination_c39_dengrogram_current <- as.dendrogram(dynamic_time_wapping_hclust_vaccination_c39_", method_current, ")") 
  # running 
  eval(parse(text = dynamic_time_wapping_hclust_vaccination_dengrogram_text))    

  
  # pointwise_absolute_distance_hclust
  # text
  pointwise_absolute_distance_hclust_vaccination_dengrogram_text <- paste0("pointwise_absolute_distance_hclust_vaccination_c39_dengrogram_current <- as.dendrogram(pointwise_absolute_distance_hclust_vaccination_c39_", method_current, ")") 
  # running 
  eval(parse(text = pointwise_absolute_distance_hclust_vaccination_dengrogram_text))    
  
  
  # cross_correlation_distance_hclust
  # text
  cross_correlation_distance_hclust_vaccination_dengrogram_text <- paste0("cross_correlation_distance_hclust_vaccination_c39_dengrogram_current <- as.dendrogram(cross_correlation_distance_hclust_vaccination_c39_", method_current, ")") 
  # running 
  eval(parse(text = cross_correlation_distance_hclust_vaccination_dengrogram_text))    
  
  
  # strain_distrubution_distance_hclust
  # text
  strain_distrubution_distance_hclust_genetics_dengrogram_text <- paste0("strain_distrubution_distance_hclust_genetics_c39_dengrogram_current <- as.dendrogram(strain_distrubution_distance_hclust_genetics_c39_", method_current, ")") 
  # running 
  eval(parse(text = strain_distrubution_distance_hclust_genetics_dengrogram_text))    
  
  
  
  
  # Comparing using -> library(dendextend)

  # trees comparison
  # Running pairwise plots from the design table
  for ( index_current in c(1:dim(design_frame)[1]) )
  {
    # Debugging 
    # index_current <-  1
    
    # texts to run
    dend1_current_text <- paste0("dend1_current <- ", design_frame$dend1[index_current])
    dend2_current_text <- paste0("dend2_current <- ", design_frame$dend2[index_current]) 
    # running texts 
    eval(parse(text = dend1_current_text))
    eval(parse(text = dend2_current_text))    
    
    
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_trees_", design_frame$file1[index_current],"_vs_", design_frame$file2[index_current],"_", method_current, "_c39.pdf", sep = ""), height = 7.25, width = 12)
    
    # Parameters
    par(cex=1, mar=c(2, 1, 1, 9) )
    # Plots
    dend_diff(dend1_current, 
              dend2_current)
    
    # main = c("11", "22") )
    
    mtext(text = paste0(design_frame$title1[index_current]," vs ", design_frame$title2[index_current], "\n(", method_current, ")" ),
          side = 3,
          line = - 2.5,
          cex = 1.35, 
          outer = TRUE)
    
    dev.off()

    
  # End of -> for ( index_current in c(1:dim(design_frame)[1]) )
  }
  

  
  
  
  
  # trees comparison part 2
  # Running pairwise plots from the design table
  for ( index_current in c(1:dim(design_frame)[1]) )
  {
    # Debugging 
    # index_current <-  1
    
    # texts to run
    dend1_current_text <- paste0("dend1_current <- ", design_frame$dend1[index_current])
    dend2_current_text <- paste0("dend2_current <- ", design_frame$dend2[index_current]) 
    # running texts 
    eval(parse(text = dend1_current_text))
    eval(parse(text = dend2_current_text))    
    
    
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_trees2_", design_frame$file1[index_current],"_vs_", design_frame$file2[index_current],"_", method_current, "_c39.pdf", sep = ""), height = 7.25, width = 12)
    

    # Customization
    # https://r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend.html
    
    # Custom these dendrograms while placing them in a list
    dend1_dend2_list <- dendlist(
      dend1_current %>% 
        set("labels_col", value = c("darkorange", "blue", "darkgoldenrod4", "darkcyan", "red"), k = 5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("darkorange", "blue", "darkgoldenrod4", "darkcyan", "red"), k = 5),
      dend2_current %>% 
        set("labels_col", value = c("forestgreen", "mediumpurple3", "yellowgreen", "violetred3", "steelblue4"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("forestgreen", "mediumpurple3", "yellowgreen", "violetred3", "steelblue4"), k = 5)
    )
    
    # Plot them together
    # Parameters
    par(cex=1, mar=c(2, 1, 1, 9) )
    # Plots
    dend_diff(dend1_dend2_list)

    mtext(text = paste0(design_frame$title1[index_current]," vs ", design_frame$title2[index_current], "\n(", method_current, ")" ),
          side = 3,
          line = - 2.5,
          cex = 1.35, 
          outer = TRUE)
    
    dev.off()
    
    
    # End of -> for ( index_current in c(1:dim(design_frame)[1]) )
  }
  
  
  
  

  # trees comparison part 3
  # Running pairwise plots from the design table
  for ( index_current in c(1:dim(design_frame)[1]) )
  {
    # Debugging 
    # index_current <-  1
    
    # texts to run
    dend1_current_text <- paste0("dend1_current <- ", design_frame$dend1[index_current])
    dend2_current_text <- paste0("dend2_current <- ", design_frame$dend2[index_current]) 
    # running texts 
    eval(parse(text = dend1_current_text))
    eval(parse(text = dend2_current_text))    
    
    
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_trees3_", design_frame$file1[index_current],"_vs_", design_frame$file2[index_current],"_", method_current, "_c39.pdf", sep = ""), height = 7.25, width = 12)
    
    # Keeping the originals
    # dend1_current_backup <- dend1_current
    # dend2_current_backup <- dend2_current
    
    
    # frame_labels1 <- data.frame( country = labels(dend1_current) )
    # merge_labels1 <- merge( x = frame_labels1, y = travel_meta_countries, by.x = "country", by.y = "country", sort = FALSE) 

    # frame_labels2 <- data.frame( country = labels(dend2_current) )
    # merge_labels2 <- merge( x = frame_labels2, y = travel_meta_countries, by.x = "country", by.y = "country", sort = FALSE) 
    
    # Updating lables
    # labels(dend1_current) <- merge_labels1$code
    # labels(dend2_current) <- merge_labels2$code
    
    # Customization
    # https://r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend.html
    
    # Custom these dendrograms while placing them in a list
    dend1_dend2_list <- dendlist(
      dend1_current %>% 
        set("labels_col", value = c("darkorange", "blue", "darkgoldenrod4", "darkcyan", "red"), k = 5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("darkorange", "blue", "darkgoldenrod4", "darkcyan", "red"), k = 5),
      dend2_current %>% 
        set("labels_col", value = c("forestgreen", "mediumpurple3", "yellowgreen", "violetred3", "steelblue4"), k=5) %>%
        set("branches_lty", 1) %>%
        set("branches_k_color", value = c("forestgreen", "mediumpurple3", "yellowgreen", "violetred3", "steelblue4"), k = 5)
    )
    
    # Plot them together
    tanglegram(dend1_dend2_list, 
               # common_subtrees_color_lines = TRUE,
               common_subtrees_color_lines = FALSE,
               highlight_distinct_edges  = TRUE, 
               highlight_branches_lwd = FALSE, 
               edge.lwd = 1.75,
               lab.cex = 1.35,
               lwd = 1.5,
               margin_top = 3,
               margin_bottom = 3,
               margin_inner = 13.5,
               margin_outer = 1.25
    )
  
    

    # Customization
    # http://talgalili.github.io/dendextend/reference/tanglegram.html
    # tanglegram(dend1_current, 
    #           dend2_current, 
    #           common_subtrees_color_branches = TRUE,
    #           highlight_distinct_edges = FALSE,
    #           edge.lwd = 3,
    #           k_branches = 5,
    #           margin_top = 3,
    #           branches_k_color = c("skyblue", "orange", "grey"),
    #           margin_bottom = 3,
    #           margin_inner = 10.25,
    #           margin_outer = 1
    #           )
    

    mtext(text = paste0(design_frame$title1[index_current]," vs ", design_frame$title2[index_current], "\n(", method_current, ")" ),
          side = 3,
          line = - 2.5,
          cex = 1.35, 
          outer = TRUE)
    
    dev.off()

    
  # End of -> for ( index_current in c(1:dim(design_frame)[1]) )
  }
  
  
  
  
  
  
  
  
  # ploting the Fowlkes-Mallows Index of two dendrogram for various k's
  # Running pairwise plots from the design table
  for ( index_current in c(1:dim(design_frame)[1]) )
  {
    # Debugging 
    # index_current <-  1
    
    # texts to run
    dend1_current_text <- paste0("dend1_current <- ", design_frame$dend1[index_current])
    dend2_current_text <- paste0("dend2_current <- ", design_frame$dend2[index_current]) 
    # running texts 
    eval(parse(text = dend1_current_text))
    eval(parse(text = dend2_current_text))    
    
    # Returns an error for medians 
    if ( (design_frame$same_elements[index_current] == 1) && (method_current != "median") )
    {
       
      # Generating pdf output.
      pdf(paste("../Plots/part09_01_fm_plot_", design_frame$file1[index_current],"_vs_", design_frame$file2[index_current],"_", method_current, "_c39.pdf", sep = ""), height = 7, width = 11)
      
      Bk_plot(tree1 = dend1_current,
              tree2 = dend2_current, 
              # rejection_line_permutation = FALSE,
              rejection_line_permutation = TRUE,
              col_line_Bk	= "black",
              # k = c(1:37),
              col_line_asymptotic		= "darkorange",
              col_line_permutation 	= "blue4", lwd = 4,
              xlab = "k (Number of Clusters)",
              main = paste0("Fowlkes-Mallows Index (", method_current, ")\n", design_frame$title1[index_current]," vs ", design_frame$title2[index_current]) 
      )
      
      
      legend( x = "topright",
              inset = c(0.08, 0.04),
              legend = c( "Bk Line", "RR Asymptomatic", "RR Permutation"),
              col = "black",
              fill = c("black", "darkorange", "blue4"),
              pt.cex = c(4, 2),
              # pch = c(19, 20),
              cex = 1.2,
              #ncol = 3
      )
      
      
      dev.off()

    # End of -> if (design_frame$same_elements == 1)  
    }
        

    
  # End of -> for ( index_current in c(1:dim(design_frame)[1]) )
  }
  
  
  
  
  
  
  
  
  # Putting histograms in a list
  # Only those which have 37 elements
  design_frame_no_travel <- design_frame[design_frame$same_elements == 1, ]
  
  # Exacting vector of names
  
  design_frame_collapsed <- unique(data.frame( rbind( as.matrix(design_frame_no_travel[,c("dend1","title1")]), 
                                                      as.matrix(design_frame_no_travel[,c("dend2","title2")]) ) ) )
  
  
  vector_of_names <- design_frame_collapsed$dend1[order(design_frame_collapsed$title1)]
  
  # text to execute
  dendlist_text_raw <- paste0(vector_of_names, collapse = ", ") 
  dendlist_text <- paste0("dendlist_of_dendrograms <- dendlist(", dendlist_text_raw ,")") 
  # running 
  eval(parse(text = dendlist_text))    
  
  # Fixing names for the dendlist
  frame_of_names <- data.frame( name = vector_of_names )
  design_frame_subset <- distinct(  data.frame( rbind( as.matrix(design_frame[,c("dend1","title1")]), 
                                                       as.matrix(design_frame[,c("dend2","title2")]) ) ) )
  frame_of_names_merged <- merge( x = frame_of_names, y =  design_frame_subset, by.x = "name", by.y = "dend1", sort = FALSE, all.y = FALSE) 

  # Adding names
  names(dendlist_of_dendrograms) <- frame_of_names_merged$title1
    
  # list of visualizations
  # visualizations_list <- c("circle", "square", "ellipse", "number", "shade", "color", "pie")
  # Fix 2023.02.01
  visualizations_list <- c("circle", "number", "pie")
  
  # list of correlations
  correlations_list = c("cophenetic", "baker", "common_nodes")
  # correlations_list = c("cophenetic", "baker", "common_nodes", "FM_index")
  
  
  # Looping to save over correlations
  for ( correlation_current in c(1:length(correlations_list)) )
  {
    # Debugging 
    # correlation_current <-  1

    # Computing current correlation
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = correlations_list[correlation_current] )
    
    
    for ( visualization_current in c(1:length(visualizations_list)) )
    {
      # Debugging 
      # visualization_current <-  1
      
      
      
      # Generating pdf output.
      pdf(paste("../Plots/part09_01_corrplot_all_lower_", method_current, "_", correlations_list[correlation_current], "_", visualizations_list[visualization_current], "_pearson_c39.pdf", sep = ""), height = 10, width = 10)
      
      corrplot(corr = cor_dendlist_current, 
               method = visualizations_list[visualization_current], 
               type   = "lower",
               tl.col = "black",
               tl.cex	= 1.25,
               cl.cex	= 1.25 )
      
      mtext(text = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = correlations_list[correlation_current] )), ")"),
            side = 3,
            line = - 2.5,
            cex = 1.35, 
            outer = TRUE)
            
            
      dev.off()
            
            
      # Generating pdf output.
      pdf(paste("../Plots/part09_01_corrplot_all_full_", method_current, "_", correlations_list[correlation_current], "_", visualizations_list[visualization_current], "_pearson_c39.pdf", sep = ""), height = 10, width = 10)
            
      
      corrplot(corr = cor_dendlist_current, 
               method = visualizations_list[visualization_current], 
               type   = "full",
               tl.col = "black",
               tl.cex	= 1.25,
               cl.cex	= 1.25 )
      
      mtext(text = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = correlations_list[correlation_current] )), ")"),
            side = 3,
            line = - 2.5,
            cex = 1.35, 
            outer = TRUE)


      dev.off()
            
            
            
    # End of -> for ( visualization_current in c(1:length(visualizations_list)) )   
    }  
    
    
    
  # End of -> for ( correlation_current in c(1:length(correlations_list)) )  
  }  
  
  
  # Fix 2023.02.15.
  # Combined verison of cophenetic and baker
  
  for ( visualization_current in c(1:length(visualizations_list)) )
  {
    # Debugging 
    # visualization_current <-  1
    
    # lower
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_corrplot_all_lower_", method_current, "_cophenetic_and_baker_" , visualizations_list[visualization_current], "_pearson_c39.pdf", sep = ""), height = 10, width = 20)
    # par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
    par(mfrow = c(1, 2))
    
    # Plot A
    
    # cophenetic
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "cophenetic" )
    
    # Exporting CSV files
    cor_dendlist_current_csv_path = paste0("../R_Output/part09_01_correlation_cophenetic_ccd_", method_current, "_c39.csv")
    cor_dendlist_current_frame <- data.frame(Data = rownames(cor_dendlist_current), cor_dendlist_current)
    names(cor_dendlist_current_frame) <- c("Data", rownames(cor_dendlist_current))
    write.table(x = cor_dendlist_current_frame, file = cor_dendlist_current_csv_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "lower",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             # mar = c(0, 0, 0, 0),
             # cex.main = 2,
             # oma = c(0, 0, 0, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")")
    )
    
    # Label A
    label_function(label_value = "A", label_cex = 6)
    
    
    
    # Plot B
    
    # baker
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "baker" )
    
    # Exporting CSV files
    cor_dendlist_current_csv_path = paste0("../R_Output/part09_01_correlation_baker_ccd_", method_current, "_c39.csv")
    cor_dendlist_current_frame <- data.frame(Data = rownames(cor_dendlist_current), cor_dendlist_current)
    names(cor_dendlist_current_frame) <- c("Data", rownames(cor_dendlist_current))
    write.table(x = cor_dendlist_current_frame, file = cor_dendlist_current_csv_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "lower",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             # mar = c(0, 0, 0, 0),
             # cex.main = 2,
             # oma = c(0, 0, 0, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    )
    
    # main-text line with speces for symmetry
    main_text_line <- paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")",
                             "                              ",
                             "Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    
    mtext(text = main_text_line,
          side = 3,
          line = - 3.75,
          cex = 2.5, 
          outer = TRUE)
    
    # Label B
    label_function(label_value = "B", label_cex = 6)
    
    # closing triangular correlation plots for two approaches (cophenetic and baker).
    dev.off()
    
    
    


    # upper        
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_corrplot_all_upper_", method_current, "_cophenetic_and_baker_" , visualizations_list[visualization_current], "_pearson_c39.pdf", sep = ""), height = 10, width = 20)
    # par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
    par(mfrow = c(1, 2))
    
    # Plot A
    
    # cophenetic
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "cophenetic" )
    
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "upper",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             mar = c(0, 0, 4.5, 0),
             # cex.main = 2,
             # oma = c(0, 0, 2, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")")
    )
    
    # Label A
    label_function(label_value = "A", label_cex = 6)
    
    
    
    # Plot B
    
    # baker
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "baker" )
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "upper",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             mar = c(0, 0, 4.5, 0),
             # cex.main = 2,
             # oma = c(0, 0, 0, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    )
    
    # main-text line with speces for symmetry
    main_text_line <- paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")",
                             "                              ",
                             "Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    
    mtext(text = main_text_line,
          side = 3,
          line = - 3.75,
          cex = 2.5, 
          outer = TRUE)
    
    # Label B
    label_function(label_value = "B", label_cex = 6)
    
    # closing triangular correlation plots for two approaches (cophenetic and baker).
    dev.off()
    
    
    
    
    # full        
    
    # Generating pdf output.
    pdf(paste("../Plots/part09_01_corrplot_all_full_", method_current, "_cophenetic_and_baker_" , visualizations_list[visualization_current], "_pearson_c39.pdf", sep = ""), height = 10, width = 20)
    # par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
    par(mfrow = c(1, 2))
    
    # Plot A
    
    # cophenetic
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "cophenetic" )
    
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "full",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             mar = c(0, 0, 4.5, 0),
             # cex.main = 2,
             # oma = c(0, 0, 0, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")")
    )
    
    # Label A
    label_function(label_value = "A", label_cex = 6)
    
    
    
    # Plot B
    
    # baker
    cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "baker" )
    
    corrplot(corr = cor_dendlist_current, 
             method = visualizations_list[visualization_current], 
             type   = "full",
             tl.col = "black",
             tl.cex	= 1.2,
             cl.cex	= 1.2,
             mar = c(0, 0, 4.5, 0),
             # cex.main = 2,
             # oma = c(0, 0, 0, 0),
             # title = paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    )
    
    # main-text line with speces for symmetry
    main_text_line <- paste0("Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "cophenetic" )), ")",
                             "                              ",
                             "Correlations for Trees (", str_to_title(gsub( pattern = "_", replacement = " ", x = "baker" )), ")")
    
    mtext(text = main_text_line,
          side = 3,
          line = - 3.75,
          cex = 2.5, 
          outer = TRUE)
    
    # Label B
    label_function(label_value = "B", label_cex = 6)
    
    # closing triangular correlation plots for two approaches (cophenetic and baker).
    dev.off()
    
 
    
    
    
    # graph    
    
    # Going over the list of correlations from correlation_threshold_list
    for (correlation_threshold in correlation_threshold_list)
    {
      # Debugging step
      # correlation_threshold <- correlation_threshold_list[1]
      
      
      # Generating pdf output.
      # Fruchterman-Reingold (1991) algorithm
      pdf(paste("../Plots/part09_01_graph_fr_", method_current, "_cophenetic_and_baker_rho_thold_", correlation_threshold, "_pearson_c39.pdf", sep = ""), height = 10, width = 20)
      # par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
      par(mfrow = c(1, 2))
      
      # Plot A
      
      # cophenetic
      cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "cophenetic" )
      
      # Matrix of indicators (+1 and -1 are necessary to convert from TRUE/FALSE to 1/1)
      cor_dendlist_current_indicator <- (cor_dendlist_current >= correlation_threshold) + 1 - 1
      cor_dendlist_current_indicator[ (cor_dendlist_current >= correlation_threshold) ] <- cor_dendlist_current[ (cor_dendlist_current >= correlation_threshold) ]
      # Extra check
      # sum(!(cor_dendlist_current_indicator >0) == (cor_dendlist_current >= correlation_threshold))
      
      # Fixing names for the indicator table
      # sum(!colnames(cor_dendlist_current_indicator) == rownames(cor_dendlist_current_indicator))
      shot_names_frame <- merge( x = data.frame(title1 = colnames(cor_dendlist_current_indicator)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
      # sum(!shot_names_frame$title1 == colnames(cor_dendlist_current_indicator))
      # Fixing names
      colnames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      rownames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      
      # Creation of the graph
      cor_dendlist_current_network <- graph_from_adjacency_matrix( adjmatrix =  cor_dendlist_current_indicator, weighted = TRUE, mode = "undirected", diag = FALSE )
      
      
      plot.igraph(x = cor_dendlist_current_network, 
                  layout = layout_with_fr(cor_dendlist_current_network),    # the layout method. see the igraph documentation for details
                  # main='Organizational network example',    #specifies the title
                  vertex.label.dist = 0,            #puts the name labels slightly off the dots
                  vertex.frame.color = "black",         #the color of the border of the dots 
                  vertex.label.color = "black",        #the color of the name labels
                  vertex.label.font = 2,            #the font of the name labels
                  # vertex.label = V(cor_dendlist_current_network)$name,  #specifies the lables of the vertices. in this case the 'name' attribute is used
                  vertex.size =  27 ,
                  # vertex.color = "darkorange",
                  vertex.color = "#5BA2CB",
                  vertex.label.family	= "Helvetica",
                  vertex.label.cex = 1,
                  margin = 0,
                  edge.curved = 0.5,
                  edge.color = "gray48",
                  edge.width = E(cor_dendlist_current_network)$weight * 15,                  
                  cex.main = 7 )       
      
      title( main = as.expression(bquote("Graph of Links for " ~ rho ~ ">" ~ .(correlation_threshold) ~ "(Cophenetic)")) , 
             line = 1, cex.main = 2.5 )
      
      # Label A
      label_function(label_value = "A", label_cex = 6)
      
      
      
      # Plot B
      
      # baker
      cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "baker" )
      
      # Matrix of indicators (+1 and -1 are necessary to convert from TRUE/FALSE to 1/1)
      cor_dendlist_current_indicator <- (cor_dendlist_current >= correlation_threshold) + 1 - 1
      cor_dendlist_current_indicator[ (cor_dendlist_current >= correlation_threshold) ] <- cor_dendlist_current[ (cor_dendlist_current >= correlation_threshold) ]
      # Extra check
      # sum(!(cor_dendlist_current_indicator >0) == (cor_dendlist_current >= correlation_threshold))
      
      # Fixing names for the indicator table
      # sum(!colnames(cor_dendlist_current_indicator) == rownames(cor_dendlist_current_indicator))
      shot_names_frame <- merge( x = data.frame(title1 = colnames(cor_dendlist_current_indicator)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
      # sum(!shot_names_frame$title1 == colnames(cor_dendlist_current_indicator))
      # Fixing names
      colnames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      rownames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      
      # Creation of the graph
      cor_dendlist_current_network <- graph_from_adjacency_matrix( adjmatrix =  cor_dendlist_current_indicator, weighted = TRUE, mode = "undirected", diag = FALSE )
      
      
      plot.igraph(x = cor_dendlist_current_network, 
                  layout = layout_with_fr(cor_dendlist_current_network),    # the layout method. see the igraph documentation for details
                  # main='Organizational network example',    #specifies the title
                  vertex.label.dist = 0,            #puts the name labels slightly off the dots
                  vertex.frame.color = "black",         #the color of the border of the dots 
                  vertex.label.color = "black",        #the color of the name labels
                  vertex.label.font = 2,            #the font of the name labels
                  # vertex.label = V(cor_dendlist_current_network)$name,  #specifies the lables of the vertices. in this case the 'name' attribute is used
                  vertex.size =  27 ,
                  # vertex.color = "darkorange",
                  vertex.color = "#5BA2CB",
                  vertex.label.family	= "Helvetica",
                  vertex.label.cex = 1,
                  margin = 0,
                  edge.curved = 0.5,
                  edge.color = "gray48",
                  edge.width = E(cor_dendlist_current_network)$weight * 15,                  
                  cex.main = 7 )       
      
      title( main = as.expression(bquote("Graph of Links for " ~ rho ~ ">" ~ .(correlation_threshold) ~ " (Baker)")) , 
             line = 1, cex.main = 2.5 )
      
      # Label B
      label_function(label_value = "B", label_cex = 6)
      
      # closing triangular correlation plots for two approaches (cophenetic and baker).
      dev.off()


      # Generating pdf output.
      # Kamada & Kawai (1989)  algorithm
      pdf(paste("../Plots/part09_01_graph_kk_", method_current, "_cophenetic_and_baker_rho_thold_", correlation_threshold, "_pearson_c39.pdf", sep = ""), height = 10, width = 20)
      # par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
      par(mfrow = c(1, 2))
      
      # Plot A
      
      # cophenetic
      cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "cophenetic" )
      
      # Matrix of indicators (+1 and -1 are necessary to convert from TRUE/FALSE to 1/1)
      cor_dendlist_current_indicator <- (cor_dendlist_current >= correlation_threshold) + 1 - 1
      cor_dendlist_current_indicator[ (cor_dendlist_current >= correlation_threshold) ] <- cor_dendlist_current[ (cor_dendlist_current >= correlation_threshold) ]
      # Extra check
      # sum(!(cor_dendlist_current_indicator >0) == (cor_dendlist_current >= correlation_threshold))
      
      # Fixing names for the indicator table
      # sum(!colnames(cor_dendlist_current_indicator) == rownames(cor_dendlist_current_indicator))
      shot_names_frame <- merge( x = data.frame(title1 = colnames(cor_dendlist_current_indicator)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
      # sum(!shot_names_frame$title1 == colnames(cor_dendlist_current_indicator))
      # Fixing names
      colnames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      rownames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      
      # Creation of the graph
      cor_dendlist_current_network <- graph_from_adjacency_matrix( adjmatrix =  cor_dendlist_current_indicator, weighted = TRUE, mode = "undirected", diag = FALSE )
      
      
      plot.igraph(x = cor_dendlist_current_network, 
                  layout = layout_with_kk(cor_dendlist_current_network),    # the layout method. see the igraph documentation for details
                  # main='Organizational network example',    #specifies the title
                  vertex.label.dist = 0,            #puts the name labels slightly off the dots
                  vertex.frame.color = "black",         #the color of the border of the dots 
                  vertex.label.color = "black",        #the color of the name labels
                  vertex.label.font = 2,            #the font of the name labels
                  # vertex.label = V(cor_dendlist_current_network)$name,  #specifies the lables of the vertices. in this case the 'name' attribute is used
                  vertex.size =  27 ,
                  # vertex.color = "darkorange",
                  vertex.color = "#5BA2CB",
                  vertex.label.family	= "Helvetica",
                  vertex.label.cex = 1,
                  margin = 0,
                  edge.curved = 0.5,
                  edge.color = "gray48",
                  edge.width = E(cor_dendlist_current_network)$weight * 15,                  
                  cex.main = 7 )       
      
      title( main = as.expression(bquote("Graph of Links for " ~ rho ~ ">" ~ .(correlation_threshold) ~ "(Cophenetic)")) , 
             line = 1, cex.main = 2.5 )
      
      # Label A
      label_function(label_value = "A", label_cex = 6)
      
      
      
      # Plot B
      
      # baker
      cor_dendlist_current <- cor.dendlist(dend = dendlist_of_dendrograms, method = "baker" )
      
      # Matrix of indicators (+1 and -1 are necessary to convert from TRUE/FALSE to 1/1)
      cor_dendlist_current_indicator <- (cor_dendlist_current >= correlation_threshold) + 1 - 1
      cor_dendlist_current_indicator[ (cor_dendlist_current >= correlation_threshold) ] <- cor_dendlist_current[ (cor_dendlist_current >= correlation_threshold) ]
      # Extra check
      # sum(!(cor_dendlist_current_indicator >0) == (cor_dendlist_current >= correlation_threshold))
      
      # Fixing names for the indicator table
      # sum(!colnames(cor_dendlist_current_indicator) == rownames(cor_dendlist_current_indicator))
      shot_names_frame <- merge( x = data.frame(title1 = colnames(cor_dendlist_current_indicator)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
      # sum(!shot_names_frame$title1 == colnames(cor_dendlist_current_indicator))
      # Fixing names
      colnames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      rownames(cor_dendlist_current_indicator) <- shot_names_frame$title1_short
      
      # Creation of the graph
      cor_dendlist_current_network <- graph_from_adjacency_matrix( adjmatrix =  cor_dendlist_current_indicator, weighted = TRUE, mode = "undirected", diag = FALSE )
      
      
      plot.igraph(x = cor_dendlist_current_network, 
                  layout = layout_with_kk(cor_dendlist_current_network),    # the layout method. see the igraph documentation for details
                  # main='Organizational network example',    #specifies the title
                  vertex.label.dist = 0,            #puts the name labels slightly off the dots
                  vertex.frame.color = "black",         #the color of the border of the dots 
                  vertex.label.color = "black",        #the color of the name labels
                  vertex.label.font = 2,            #the font of the name labels
                  # vertex.label = V(cor_dendlist_current_network)$name,  #specifies the lables of the vertices. in this case the 'name' attribute is used
                  vertex.size =  27 ,
                  # vertex.color = "darkorange",
                  vertex.color = "#5BA2CB",
                  vertex.label.family	= "Helvetica",
                  vertex.label.cex = 1,
                  margin = 0,
                  edge.curved = 0.5,
                  edge.color = "gray48",
                  edge.width = E(cor_dendlist_current_network)$weight * 15,                  
                  cex.main = 7 )       
      
      title( main = as.expression(bquote("Graph of Links for " ~ rho ~ ">" ~ .(correlation_threshold) ~ " (Baker)")) , 
             line = 1, cex.main = 2.5 )
      
      # Label B
      label_function(label_value = "B", label_cex = 6)
      
      # closing triangular correlation plots for two approaches (cophenetic and baker).
      dev.off()
      
      
    # End of -> for (correlation_threshold in correlation_threshold_list)  
    }  
      

  # End of -> for ( visualization_current in c(1:length(visualizations_list)) )   
  }  
  
  
  
  
    
  
  robinson_foulds_current <- as.matrix(dist.dendlist(dendlist_of_dendrograms))
  robinson_foulds_standardized_current <- as.matrix(dist.dendlist(dendlist_of_dendrograms))/max(as.matrix(dist.dendlist(dendlist_of_dendrograms))) * 1000
  
  
  # Robinson-Foulds distance (also known as symmetric difference) between two dendrograms.
  sink(paste0("../R_Output/part09_01_rob_foulds_ccd_", method_current, "_c39.txt"))
  
  # Distance
  cat("\nRobinson-Foulds Distance -> ",  method_current, "\n")
  print( robinson_foulds_current )

  # Standardized Distance
  cat("\n\n\n\n\n\nRobinson-Foulds Distance (Standardized 0-1000) -> ",  method_current, "\n")
  print( robinson_foulds_standardized_current  )
  
  sink()
  

  # Exporting CSV files
  # Regular
  robinson_foulds_current_csv_path = paste0("../R_Output/part09_01_rob_foulds_ccd_", method_current, "_c39.csv")
  write.table(x = robinson_foulds_current, file = robinson_foulds_current_csv_path, sep = ",", row.names = TRUE)
  # Standardized
  robinson_foulds_standardized_current_csv_path = paste0("../R_Output/part09_01_rob_foulds_ccd_standardized_", method_current, "_c39.csv")
  write.table(x = robinson_foulds_standardized_current, file = robinson_foulds_standardized_current_csv_path, sep = ",", row.names = TRUE)
  

  # Exporting TEX files
  # Regular
  # Changing names
  shot_names_frame2 <- merge( x = data.frame(title1 = colnames(robinson_foulds_current)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
  # sum(!colnames(robinson_foulds_current) == shot_names_frame2$title1)
  colnames(robinson_foulds_current) <- shot_names_frame2$title1_short
  rownames(robinson_foulds_current) <- colnames(robinson_foulds_current)
  # Creating xtable object
  robinson_foulds_current_xtable <- xtable(x = robinson_foulds_current, digits = 0 )  
  # Exporting as tex file
  # Creating a path 
  robinson_foulds_current_xtable_path_out <- paste0("../R_Output/part09_01_rob_foulds_ccd_", method_current, "_c39.tex")
  # Printing
  print.xtable( x = robinson_foulds_current_xtable, type="latex", file = robinson_foulds_current_xtable_path_out, include.rownames = TRUE )
  # Standardized
  # Changing names
  shot_names_frame2 <- merge( x = data.frame(title1 = colnames(robinson_foulds_standardized_current)), y = frame_names_unique_ordered, by = "title1", sort = FALSE )
  # sum(!colnames(robinson_foulds_standardized_current) == shot_names_frame2$title1)
  colnames(robinson_foulds_standardized_current) <- shot_names_frame2$title1_short
  rownames(robinson_foulds_standardized_current) <- colnames(robinson_foulds_standardized_current)
  # Creating xtable object
  robinson_foulds_standardized_current_xtable <- xtable(x = robinson_foulds_standardized_current, digits = 2 )  
  # Exporting as tex file
  # Creating a path 
  robinson_foulds_standardized_current_xtable_path_out <- paste0("../R_Output/part09_01_rob_foulds_ccd_standardized_", method_current, "_c39.tex")
  # Printing
  print.xtable( x = robinson_foulds_standardized_current_xtable, type="latex", file = robinson_foulds_standardized_current_xtable_path_out, include.rownames = TRUE )

  
# End of -> for (method_current in hclust_methods)
} 



# Listing available objects
ls()





