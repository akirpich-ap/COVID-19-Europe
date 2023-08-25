# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.20. ask
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

# This package is used to produce latex tables.
# library(xtable)



# Reading data
load(file = paste("../R_Data/countries_travel.RData"))
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


# Creating matrices of distances
# Extracting dimensions
matrix_dim <- length(countries_travel)


# travel_statistic_2016
matrix_distance_travel_statistic_2016 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
colnames(matrix_distance_travel_statistic_2016) = countries_travel
rownames(matrix_distance_travel_statistic_2016) = countries_travel

# Getting minimum summaries 
values_duplicated_statistic_2016 <- pairs_frame_travel$travel_statistic_2016
values_duplicated_statistic_2016 <-  values_duplicated_statistic_2016 / max(values_duplicated_statistic_2016)

min_values_duplicated_statistic_2016    <-  min(values_duplicated_statistic_2016)
max_values_duplicated_statistic_2016    <-  max(values_duplicated_statistic_2016)
mean_values_duplicated_statistic_2016   <-  mean(values_duplicated_statistic_2016)


# Filling the matrix
for (current_country1 in countries_travel)
{
  for (current_country2 in countries_travel)
  {
    # Debugging step
    # current_country1 = countries_travel[length(countries_travel)]
    # current_country2 = countries_travel[2]

    # current_country1 = countries_travel[35]
    # current_country2 = countries_travel[15]


    # Extracting row index
    row_index = which((pairs_frame_travel$Country1 == current_country1) * (pairs_frame_travel$Country2 == current_country2) +
      (pairs_frame_travel$Country1 == current_country2) * (pairs_frame_travel$Country2 == current_country1) == 1)

    # Filling the value
    if (current_country1 != current_country2) {

      matrix_distance_travel_statistic_2016[current_country1, current_country2] = 
        # (max(pairs_frame_travel$travel_statistic_2016) - pairs_frame_travel$travel_statistic_2016[row_index])/median(pairs_frame_travel$travel_statistic_2016)
        # max(pairs_frame_travel$travel_statistic_2016) - pairs_frame_travel$travel_statistic_2016[row_index]
        # quantile(x = pairs_frame_travel$travel_statistic_2016, probs = c(0.99)) - pairs_frame_travel$travel_statistic_2016[row_index]
        exp(- (values_duplicated_statistic_2016[row_index] - min_values_duplicated_statistic_2016)/mean_values_duplicated_statistic_2016  )

    }


    # End of -> for ( current_country2 in countries )
  }

  # End of -> for ( current_country1 in countries )
}

# Fix 2022.12.05.
# Standardizing.
matrix_distance_travel_statistic_2016 <- matrix_distance_travel_statistic_2016/max(matrix_distance_travel_statistic_2016) * 1000


# Saving the processed results as CSV files.
# Exporting 
matrix_distance_travel_statistic_2016_csv_path = paste("../R_Output/matrix_distance_travel_statistic_2016.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(matrix_distance_travel_statistic_2016)), matrix_distance_travel_statistic_2016),
            file = matrix_distance_travel_statistic_2016_csv_path, sep = ",", row.names = FALSE)




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
# Standardizing.
matrix_distance_travel_statistic_average_2011_2016 <- matrix_distance_travel_statistic_average_2011_2016/max(matrix_distance_travel_statistic_average_2011_2016) * 1000


# Saving the processed results as CSV files.
# Exporting 
matrix_distance_travel_statistic_average_2011_2016_csv_path = paste("../R_Output/matrix_distance_travel_statistic_average_2011_2016.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(matrix_distance_travel_statistic_average_2011_2016)), matrix_distance_travel_statistic_average_2011_2016),
            file = matrix_distance_travel_statistic_average_2011_2016_csv_path, sep = ",", row.names = FALSE)





# travel_statistic_2016
matrix_distance_points_travel_statistic_2016 = cmdscale(matrix_distance_travel_statistic_2016)
x_values_travel_statistic_2016 = matrix_distance_points_travel_statistic_2016[, 1]
y_values_travel_statistic_2016 = matrix_distance_points_travel_statistic_2016[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part05_02_travel_counts_multidimensional_scaling_plots_travel_statistic_2016", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_travel_statistic_2016,
  y = y_values_travel_statistic_2016,
  col = "firebrick2",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Clustering Based on Pairwise Distances Derived from Travel Data (2016)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_travel_statistic_2016),
    max(x_values_travel_statistic_2016) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_travel_statistic_2016),
    max(y_values_travel_statistic_2016) * 1.1
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
text(x = x_values_travel_statistic_2016, y = y_values_travel_statistic_2016, labels = rownames(matrix_distance_points_travel_statistic_2016), srt=45,  cex = 1.3)
dev.off()




# travel_statistic_2016
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_travel_statistic_2016 = hclustfunc(matrix_distance_travel_statistic_2016, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "travel_matrix_hclust_2016_", method_current, " <- cluster_travel_statistic_2016" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/travel_matrix_hclust_2016_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( travel_matrix_hclust_2016_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part05_02_travel_counts_multidimensional_scaling_plots_clusters_travel_statistic_2016_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_travel_statistic_2016,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "travel_statistic_2016 (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_travel_statistic_2016),
       #           max(x_values_travel_statistic_2016) * 1.1  ),
       # ylim = c( -0.5*max(cluster_travel_statistic_2016$height), max(cluster_travel_statistic_2016$height) ),
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
  mtext( paste0("Clustering Based on Pairwise Distances Derived from Travel Data (2016) - ", method_current),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}








# travel_statistic_average_2011_2016
matrix_distance_points_travel_statistic_average_2011_2016 = cmdscale(matrix_distance_travel_statistic_average_2011_2016)
x_values_travel_statistic_average_2011_2016 = matrix_distance_points_travel_statistic_average_2011_2016[, 1]
y_values_travel_statistic_average_2011_2016 = matrix_distance_points_travel_statistic_average_2011_2016[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part05_02_travel_counts_multidimensional_scaling_plots_travel_statistic_average_2011_2016", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_travel_statistic_average_2011_2016,
  y = y_values_travel_statistic_average_2011_2016,
  col = "firebrick2",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Clustering Based on Pairwise Distances Derived from Travel Data (Average of 2011-2016)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_travel_statistic_average_2011_2016),
    max(x_values_travel_statistic_average_2011_2016) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_travel_statistic_average_2011_2016),
    max(y_values_travel_statistic_average_2011_2016) * 1.1
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
text(x = x_values_travel_statistic_average_2011_2016, y = y_values_travel_statistic_average_2011_2016, labels = rownames(matrix_distance_points_travel_statistic_average_2011_2016), srt=45,  cex = 1.3)
dev.off()




# travel_statistic_average_2011_2016
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_travel_statistic_average_2011_2016 = hclustfunc(matrix_distance_travel_statistic_average_2011_2016, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "travel_matrix_hclust_average_2011_2016_", method_current, " <- cluster_travel_statistic_average_2011_2016" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/travel_matrix_hclust_average_2011_2016_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( travel_matrix_hclust_average_2011_2016_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part05_02_travel_counts_multidimensional_scaling_plots_clusters_travel_statistic_average_2011_2016_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_travel_statistic_average_2011_2016,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "travel_statistic_average_2011_2016 (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_travel_statistic_average_2011_2016),
       #           max(x_values_travel_statistic_average_2011_2016) * 1.1  ),
       # ylim = c( -0.5*max(cluster_travel_statistic_average_2011_2016$height), max(cluster_travel_statistic_average_2011_2016$height) ),
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
  mtext( paste0("Clustering Based on Pairwise Distances Derived from Travel Data (Average of 2011-2016) - ", method_current),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}


