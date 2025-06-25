# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.30. ask
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
# vaccination from RData file.
load(file = paste("../R_Data/vaccination_data_frame_standardized.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()


hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")


# Printing summaries
tail(vaccination_data_frame_standardized)


# For entire Europe extension
graph_suffix = "_eu"

# Creating a frame of pairs
pairs_frame = combinations(n = length(countries), r = 2, v = countries, repeats.allowed = FALSE)



# Loading Kolmogorov-Smirnoff statistics
# vaccination
load(file = paste("../R_Data/pairs_frame_vaccination_cc.RData"))
ls()


# Creating matrices of distances
# Extracting dimensions
matrix_dim = length(countries)

# vaccination
cc_distance_matrix_vaccination_c39 = matrix(0, nrow = matrix_dim, ncol = matrix_dim)
colnames(cc_distance_matrix_vaccination_c39) = countries
rownames(cc_distance_matrix_vaccination_c39) = countries
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
    row_index = which((pairs_frame_vaccination_cc$Country1 == current_country1) * (pairs_frame_vaccination_cc$Country2 == current_country2) +
      (pairs_frame_vaccination_cc$Country1 == current_country2) * (pairs_frame_vaccination_cc$Country2 == current_country1) == 1)

    # Filling the value
    if (current_country1 != current_country2) {
      cc_distance_matrix_vaccination_c39[current_country1, current_country2] = pairs_frame_vaccination_cc$cc_statistic[row_index]
    }


    # End of -> for ( current_country2 in countries )
  }

  # End of -> for ( current_country1 in countries )
}

# Fix 2022.12.05.
# Standardizing distances
cc_distance_matrix_vaccination_c39 <- cc_distance_matrix_vaccination_c39/max(cc_distance_matrix_vaccination_c39) * 1000




# vaccination
cc_distance_points_vaccination = cmdscale(cc_distance_matrix_vaccination_c39)
x_values_vaccination = cc_distance_points_vaccination[, 1]
y_values_vaccination = cc_distance_points_vaccination[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
pdf(paste("../Plots/part07_06_vaccination_cross_correlation_distance_vaccination_c39", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_vaccination,
  y = y_values_vaccination,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "Vaccination (Cross-Correlation Distance)",
  # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
  xlim = c(
    1.1 * min(x_values_vaccination),
    max(x_values_vaccination) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_vaccination),
    max(y_values_vaccination) * 1.1
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
text(x = x_values_vaccination, y = y_values_vaccination, labels = rownames(cc_distance_points_vaccination), srt=45,  cex = 1.3)
dev.off()



# vaccination
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_vaccination = hclustfunc(cc_distance_matrix_vaccination_c39, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "cross_correlation_distance_hclust_vaccination_c39_", method_current, " <- cluster_vaccination" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/cross_correlation_distance_hclust_vaccination_c39_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( cross_correlation_distance_hclust_vaccination_c39_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part07_06_vaccination_cross_correlation_distance_clusters_vaccination_c39_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_vaccination,
    # col = "darkorange",
    # col = color_01,
    lwd = 0.5,
    # pch = 16,
    # pch = shape_01,
    # pch = 17,
    # type = "p",
    pch = 19,
    # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
    # main = "Vaccination (Cross-Correlation Distance)",
    # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
    # xlim = c( 1.1 * min(x_values_vaccination),
    #           max(x_values_vaccination) * 1.1  ),
    # ylim = c( -0.5*max(cluster_vaccination$height), max(cluster_vaccination$height) ),
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
  mtext( paste0("Vaccination (Cross-Correlation Distance) - ", method_current),
    side = 3,
    line = -3.5,
    cex = 5,
    outer = TRUE
  )
  dev.off()
}









