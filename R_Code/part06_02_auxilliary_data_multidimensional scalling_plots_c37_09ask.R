# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.20. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations.
# library(matrixStats)

# Library for the latex exports in the nice format.
# library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
# library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
# library(methods)

# Loading package required to read library(readxl)
library(readxl)

# Loading package required to read library(matrixStats)
library(matrixStats)

# package for pairwise distances
library(rdist)


# Library for permutations
library(gtools)


# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
# library(readr)

# To convert dates from text to dates
# library(lubridate)

# Loading countries
load( file = paste("../R_Data/countries.RData"))
graph_suffix = "_eu"


# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]





# Loading
load( file = paste("../R_Data/auxilliary_data_merge.RData"))
# Loading standardized
load( file = paste("../R_Data/auxilliary_data_merge_standardized.RData"))
# list of clustering methods

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")

# Picking columns auxilliary_data_merge_standardized
colnames(auxilliary_data_merge_standardized)

variables_population_health <- 
  c("life_expectancy_2019", "cardiovascular_disease_death_rate_2019", "diabetes_prevalence_2019", "share_of_obese_adults_2016")  
variables_demographics      <- 
  c("total_fertility_rate_2017",  "median_age_2015", "population_density_2018", "gdp_per_capita_2018", "human_development_index_2017", "hospital_beds_per1000_2013", "share_of_extreme_poverty_population_2016")


# countries to keep
which_countries_keep <- which(rownames(auxilliary_data_merge_standardized) %in% countries)


# Computing distances - complete  
distance_matrix_complete          <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep,], metric = "euclidean")
colnames(distance_matrix_complete)          <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_complete)          <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]


# Computing distances - population_health  
distance_matrix_population_health <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep, variables_population_health], metric = "euclidean")
colnames(distance_matrix_population_health) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_population_health) <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]


# Computing distances - demographics  
distance_matrix_demographics       <- pdist(X = auxilliary_data_merge_standardized[ which_countries_keep, variables_demographics], metric = "euclidean")
colnames(distance_matrix_demographics)      <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]
rownames(distance_matrix_demographics)      <- rownames(auxilliary_data_merge_standardized)[which_countries_keep]


# head(distance_matrix_complete)
# sqrt(   sum( (auxilliary_data_merge_standardized[1, ] - auxilliary_data_merge_standardized[2, ])^2 )  )
# sqrt(   sum( (auxilliary_data_merge_standardized[1, ] - auxilliary_data_merge_standardized[3, ])^2 )  )
# sqrt(   sum( (auxilliary_data_merge_standardized[1, ] - auxilliary_data_merge_standardized[4, ])^2 )  )

# Fix 2022.12.05
# Standardizing distances.
distance_matrix_complete          <-  distance_matrix_complete/max(distance_matrix_complete) * 1000
distance_matrix_population_health <-  distance_matrix_population_health/max(distance_matrix_population_health) * 1000
distance_matrix_demographics      <-  distance_matrix_demographics/max(distance_matrix_demographics) * 1000

# Exporting 
distance_matrix_complete_csv_path = paste("../R_Output/distance_matrix_complete_c37.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(distance_matrix_complete)), distance_matrix_complete),
            file = distance_matrix_complete_csv_path, sep = ",", row.names = FALSE)

distance_matrix_population_health_csv_path = paste("../R_Output/distance_matrix_population_health_c37.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(distance_matrix_population_health)), distance_matrix_population_health),
            file = distance_matrix_population_health_csv_path, sep = ",", row.names = FALSE)

distance_matrix_demographics_csv_path = paste("../R_Output/distance_matrix_demographics_c37.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(distance_matrix_demographics)), distance_matrix_demographics),
            file = distance_matrix_demographics_csv_path, sep = ",", row.names = FALSE)








# complete
distance_matrix_complete_points = cmdscale(distance_matrix_complete)
x_values_complete = distance_matrix_complete_points[, 1]
y_values_complete = distance_matrix_complete_points[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_complete_c37", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_complete,
  y = y_values_complete,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Complete Countires Characteristics Eucledian Distance",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_complete),
    max(x_values_complete) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_complete),
    max(y_values_complete) * 1.1
  ),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  cex = 3,
  cex.axis = 1.45,
  cex.lab = 1.55,
  cex.main = 2,
  cex.sub = 2
)
text(x = x_values_complete, y = y_values_complete, labels = rownames(distance_matrix_complete_points), srt=45,  cex = 1.3)
dev.off()



# complete
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}


for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_complete = hclustfunc(distance_matrix_complete, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "auxilliary_data_hclust_complete_c37_", method_current, " <- cluster_complete" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_complete_c37_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( auxilliary_data_hclust_complete_c37_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_clusters_complete_c37_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_complete,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "complete (Kolmogorov-Smirnov Statistic)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_complete),
       #           max(x_values_complete) * 1.1  ),
       # ylim = c( -0.5*max(cluster_complete$height), max(cluster_complete$height) ),
       # ylim = c(0, y_max_value_current * 1.2  ),
       # xlab = "Time",
       ann = FALSE,
       xlab = "fdsfsd",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       cex = 3.5,
       cex.axis = 1.45,
       cex.lab = 1.55,
       cex.main = 2,
       cex.sub = 2,
       frame.plot = FALSE
  )
  # Add main title
  mtext( paste0("Complete Countires Characteristics Eucledian Distance - ", method_current),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}











# population_health
distance_matrix_population_health_points = cmdscale(distance_matrix_population_health)
x_values_population_health = distance_matrix_population_health_points[, 1]
y_values_population_health = distance_matrix_population_health_points[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_population_health_c37", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_population_health,
  y = y_values_population_health,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Population Health Countires Characteristics Eucledian Distance",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_population_health),
    max(x_values_population_health) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_population_health),
    max(y_values_population_health) * 1.1
  ),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  cex = 3,
  cex.axis = 1.45,
  cex.lab = 1.55,
  cex.main = 2,
  cex.sub = 2
)
text(x = x_values_population_health, y = y_values_population_health, labels = rownames(distance_matrix_population_health_points), srt=45,  cex = 1.3)
dev.off()



# population_health
hclustfunc = function(x, method_selected = "population_health", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}


for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_population_health = hclustfunc(distance_matrix_population_health, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "auxilliary_data_hclust_population_health_c37_", method_current, " <- cluster_population_health" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_population_health_c37_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( auxilliary_data_hclust_population_health_c37_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_clusters_population_health_c37_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_population_health,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "population_health (Kolmogorov-Smirnov Statistic)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_population_health),
       #           max(x_values_population_health) * 1.1  ),
       # ylim = c( -0.5*max(cluster_population_health$height), max(cluster_population_health$height) ),
       # ylim = c(0, y_max_value_current * 1.2  ),
       # xlab = "Time",
       ann = FALSE,
       xlab = "fdsfsd",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       cex = 3.5,
       cex.axis = 1.45,
       cex.lab = 1.55,
       cex.main = 2,
       cex.sub = 2,
       frame.plot = FALSE
  )
  # Add main title
  mtext( paste0("Population Health Countires Characteristics Eucledian Distance - ", method_current),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}











# demographics
distance_matrix_demographics_points = cmdscale(distance_matrix_demographics)
x_values_demographics = distance_matrix_demographics_points[, 1]
y_values_demographics = distance_matrix_demographics_points[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_demographics_c37", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_demographics,
  y = y_values_demographics,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Socio-Demographic Countires Characteristics Eucledian Distance",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_demographics),
    max(x_values_demographics) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_demographics),
    max(y_values_demographics) * 1.1
  ),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  cex = 3,
  cex.axis = 1.45,
  cex.lab = 1.55,
  cex.main = 2,
  cex.sub = 2
)
text(x = x_values_demographics, y = y_values_demographics, labels = rownames(distance_matrix_demographics_points), srt=45,  cex = 1.3)
dev.off()



# demographics
hclustfunc = function(x, method_selected = "demographics", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}


for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_demographics = hclustfunc(distance_matrix_demographics, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "auxilliary_data_hclust_demographics_c37_", method_current, " <- cluster_demographics" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/auxilliary_data_hclust_demographics_c37_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( auxilliary_data_hclust_demographics_c37_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part06_02_auxilliary_data_multidimensional_scaling_plots_clusters_demographics_c37_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_demographics,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "demographics (Kolmogorov-Smirnov Statistic)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_demographics),
       #           max(x_values_demographics) * 1.1  ),
       # ylim = c( -0.5*max(cluster_demographics$height), max(cluster_demographics$height) ),
       # ylim = c(0, y_max_value_current * 1.2  ),
       # xlab = "Time",
       ann = FALSE,
       xlab = "fdsfsd",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       cex = 3.5,
       cex.axis = 1.45,
       cex.lab = 1.55,
       cex.main = 2,
       cex.sub = 2,
       frame.plot = FALSE
  )
  # Add main title
  mtext( paste0("Socio-Demographic Countires Characteristics Eucledian Distance - ", method_current),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}














































