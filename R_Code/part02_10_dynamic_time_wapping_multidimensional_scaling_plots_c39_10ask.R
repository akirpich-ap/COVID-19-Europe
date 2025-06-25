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

# This is to use the capitalized names i.e. for str_to_title() 
library(stringr)

# This package is used to produce latex tables.
# library(xtable)




# Reading data
# Incidence from RData file.
load(file = paste("../R_Data/incidence_data_standardized.RData"))
# Mortality as RData file.
load(file = paste("../R_Data/mortality_data_standardized.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()


hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")


# Printing summaries
tail(incidence_data_selected_fixed_plus_standardized)
tail(mortality_data_selected_fixed_plus_standardized)

# For entire Europe extension
graph_suffix = "_eu"

# Creating a frame of pairs
# pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)



# Loading Kolmogorov-Smirnoff statistics
# incidence
load(file = paste("../R_Data/pairs_frame_incidence_tw.RData"))
# mortality
load(file = paste("../R_Data/pairs_frame_mortality_tw.RData"))
ls()


# Creating matrices of distances
# Extracting dimensions
matrix_dim = length(countries)

# incidence
tw_distance_matrix_incidence_c39 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
colnames(tw_distance_matrix_incidence_c39) = countries
rownames(tw_distance_matrix_incidence_c39) = countries
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
    if (current_country1 != current_country2) {
      tw_distance_matrix_incidence_c39[current_country1, current_country2] = pairs_frame_incidence_tw$tw_statistic[row_index]
    }
    
    
    # End of -> for ( current_country2 in countries )
  }
  
  # End of -> for ( current_country1 in countries )
}

# Fix 2022.12.05.
# Standardizing distances
tw_distance_matrix_incidence_c39 <- tw_distance_matrix_incidence_c39/max(tw_distance_matrix_incidence_c39) * 1000


# mortality
tw_distance_matrix_mortality_c39 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
colnames(tw_distance_matrix_mortality_c39) = countries
rownames(tw_distance_matrix_mortality_c39) = countries
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
    if (current_country1 != current_country2) {
      tw_distance_matrix_mortality_c39[current_country1, current_country2] = pairs_frame_mortality_tw$tw_statistic[row_index]
    }
    
    
    # End of -> for ( current_country2 in countries )
  }
  
  # End of -> for ( current_country1 in countries )
}

# Fix 2022.12.05.
# Standardizing distances
tw_distance_matrix_mortality_c39 <- tw_distance_matrix_mortality_c39/max(tw_distance_matrix_mortality_c39) * 1000




## incidence
tw_distance_points_incidence = cmdscale(tw_distance_matrix_incidence_c39)
x_values_incidence = tw_distance_points_incidence[, 1]
y_values_incidence = tw_distance_points_incidence[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_incidence_c39", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_incidence,
  y = y_values_incidence,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Incidence (Dynamic Time Wrapping Distance)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_incidence),
    max(x_values_incidence) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_incidence),
    max(y_values_incidence) * 1.1
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
text(x = x_values_incidence, y = y_values_incidence, labels = rownames(tw_distance_points_incidence), srt=45,  cex = 1.3)
dev.off()



# incidence
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_incidence = hclustfunc(tw_distance_matrix_incidence_c39, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "dynamic_time_wapping_hclust_incidence_c39_", method_current, " <- cluster_incidence" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/dynamic_time_wapping_hclust_incidence_c39_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( dynamic_time_wapping_hclust_incidence_c39_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_clusters_incidence_c39_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_incidence,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "Incidence (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_incidence),
       #           max(x_values_incidence) * 1.1  ),
       # ylim = c( -0.5*max(cluster_incidence$height), max(cluster_incidence$height) ),
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
  mtext( paste0("Incidence (Dynamic Time Wrapping Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current )) ),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}









# mortality
tw_distance_points_mortality = cmdscale(tw_distance_matrix_mortality_c39)
x_values_mortality = tw_distance_points_mortality[, 1]
y_values_mortality = tw_distance_points_mortality[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_mortality_c39", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_mortality,
  y = y_values_mortality,
  col = "firebrick1",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Mortality (Dynamic Time Wrapping Distance)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_mortality),
    max(x_values_mortality) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_mortality),
    max(y_values_mortality) * 1.1
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
text(x = x_values_mortality, y = y_values_mortality, labels = rownames(tw_distance_points_mortality), srt=45,  cex = 1.3)
dev.off()




# mortality
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_mortality = hclustfunc(tw_distance_matrix_mortality_c39, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "dynamic_time_wapping_hclust_mortality_c39_", method_current, " <- cluster_mortality" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/dynamic_time_wapping_hclust_mortality_c39_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( dynamic_time_wapping_hclust_mortality_c39_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_clusters_mortality_c39_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_mortality,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "mortality (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_mortality),
       #           max(x_values_mortality) * 1.1  ),
       # ylim = c( -0.5*max(cluster_mortality$height), max(cluster_mortality$height) ),
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
  mtext( paste0("Mortality (Dynamic Time Wrapping Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current )) ),
         side = 3,
         line = -3.5,
         cex = 5,
         outer = TRUE
  )
  dev.off()
}













# Fix 2023.02.11. ask
# Combining two. 


## incidence
tw_distance_points_incidence = cmdscale(tw_distance_matrix_incidence_c39)
x_values_incidence = tw_distance_points_incidence[, 1]
y_values_incidence = tw_distance_points_incidence[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly

# mortality
tw_distance_points_mortality = cmdscale(tw_distance_matrix_mortality_c39)
x_values_mortality = tw_distance_points_mortality[, 1]
y_values_mortality = tw_distance_points_mortality[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly




# Creating labeling function

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
  
}






# Generating pdf output.
pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_incidence_mortality_c39", graph_suffix, ".pdf", sep = ""), height = 16, width = 32)
# Defining the number of plots
par(par(mfrow = c(1, 2)), mar = c(5.5, 5.1, 5.1, 2.1))

plot(
  x = x_values_incidence,
  y = y_values_incidence,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Multidimensional Scaling for Incidence\n(Dynamic Time Wrapping Distance)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_incidence),
    max(x_values_incidence) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_incidence),
    max(y_values_incidence) * 1.1
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
text(x = x_values_incidence, y = y_values_incidence, labels = rownames(tw_distance_points_incidence), srt=45,  cex = 1.3)


# Label A
label_function(label_value = "A", label_cex = 6)



plot(
  x = x_values_mortality,
  y = y_values_mortality,
  col = "firebrick1",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Multidimensional Scaling for Mortality\n(Dynamic Time Wrapping Distance)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_mortality),
    max(x_values_mortality) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_mortality),
    max(y_values_mortality) * 1.1
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
text(x = x_values_mortality, y = y_values_mortality, labels = rownames(tw_distance_points_mortality), srt=45,  cex = 1.3)

# Label B
label_function(label_value = "B", label_cex = 6)


dev.off()















# Combined for the two hierarchical clusters

# Creating labeling function

label_function2 <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_value
  x <- x[1] + strwidth(txt, cex=4) * 15 / 5
  y <- y[2] - strheight(txt, cex=4) * 15 / 5
  text(x, y, txt, cex = label_cex )
  
}



# incidence or mortality
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_incidence = hclustfunc(tw_distance_matrix_incidence_c39, method_selected = method_current)
  cluster_mortality = hclustfunc(tw_distance_matrix_mortality_c39, method_selected = method_current)
  

  # Generating pdf output.
  pdf(paste("../Plots/part02_10_dynamic_time_wapping_multidimensional_scaling_plots_clusters_incidence_mortality_c39_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 80)
  # Defining the number of plots
  par(mfrow = c(1, 2))
  
  # Panel A
  
  plot(cluster_incidence,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste0("Incidence (Dynamic Time Wrapping Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current )) ),
       # main = "Incidence (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_incidence),
       #           max(x_values_incidence) * 1.1  ),
       # ylim = c( -0.5*max(cluster_incidence$height), max(cluster_incidence$height) ),
       # ylim = c(0, y_max_value_current * 1.2  ),
       # xlab = "Time",
       ann = FALSE,
       xlab = "fdsfsd",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       cex = 6,
       cex.axis = 1.45,
       cex.lab = 1.55,
       cex.main = 3,
       cex.sub = 2,
       frame.plot = FALSE
  )

  # Label A
  label_function2(label_value = "A", label_cex = 20)
  
  
  
  
  # Panel B
  
  plot(cluster_mortality,
       # col = "darkorange",
       # col = color_01,
       lwd = 0.5,
       # pch = 16,
       # pch = shape_01,
       # pch = 17,
       # type = "p",
       pch = 19,
       # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
       # main = "mortality (Dynamic Time Wrapping Distance)",
       # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
       # xlim = c( 1.1 * min(x_values_mortality),
       #           max(x_values_mortality) * 1.1  ),
       # ylim = c( -0.5*max(cluster_mortality$height), max(cluster_mortality$height) ),
       # ylim = c(0, y_max_value_current * 1.2  ),
       # xlab = "Time",
       ann = FALSE,
       xlab = "fdsfsd",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       cex = 6,
       cex.axis = 1.45,
       cex.lab = 1.55,
       cex.main = 3,
       cex.sub = 2,
       frame.plot = FALSE
  )

  # Add main title
  mtext(    paste0("Incidence (Dynamic Time Wrapping Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current)),
    "               Mortality (Dynamic Time Wrapping Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current))  ),
         side = 3,
         line = -10,
         cex = 8,
         outer = TRUE
  )
  
  
  
  
  # Label B
  label_function2(label_value = "B", label_cex = 20)
  
  
  
  dev.off()
  
  
# End of -> for (method_current in hclust_methods)  
}






