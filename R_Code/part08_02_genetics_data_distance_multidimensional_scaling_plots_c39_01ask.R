# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.13. ask
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


# For entire Europe extension
graph_suffix = "_eu"



# Reading data
# genetics from RData file.
load(file = paste("../R_Data/genetics_data_matrix_ordered_c39.RData"))
load(file = paste("../R_Data/countries.RData"))
ls()

hclust_methods = c("complete", "average")
# hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# Fixing missing countries i.e. dropping c("Serbia", "Montenegro")
# countries <- countries[ which( !countries %in% c("Serbia", "Montenegro") ) ]


# Fix 2023.06.05.
# Standardizing distances
genetics_data_matrix_c39 <- genetics_data_matrix_ordered_c39/max(genetics_data_matrix_ordered_c39) * 1000
# Exporting 
genetics_data_matrix_c39_csv_path = paste("../R_Output/genetics_data_matrix_c39.csv", sep = "")
write.table(x    = data.frame( country = gsub(pattern = " ", replacement = ".", x = rownames(genetics_data_matrix_c39)), genetics_data_matrix_c39),
            file = genetics_data_matrix_c39_csv_path, sep = ",", row.names = FALSE)





# genetics data
gd_distance_points_genetics = cmdscale(genetics_data_matrix_c39)
x_values_genetics = gd_distance_points_genetics[, 1]
y_values_genetics = gd_distance_points_genetics[, 2]
# reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly


# Generating pdf output.
  pdf(paste("../Plots/part08_02_strain_distribution_distance_multidimensional_scaling_plots_c39", graph_suffix, ".pdf", sep = ""), height = 16, width = 16)
# Defining the number of plots
plot(
  x = x_values_genetics,
  y = y_values_genetics,
  col = "darkorange",
  # col = color_01,
  lwd = 0.5,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "p",
  pch = 19,
  main = "Genetic Diversity (Strain Distirbution Distance)",
  xlim = c(
    1.1 * min(x_values_genetics),
    max(x_values_genetics) * 1.1
  ),
  ylim = c(
    1.1 * min(y_values_genetics),
    max(y_values_genetics) * 1.1
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
text(x = x_values_genetics, y = y_values_genetics, labels = rownames(gd_distance_points_genetics), srt=45,  cex = 1.3)
dev.off()



# genetics
hclustfunc = function(x, method_selected = "complete", dmeth = "euclidean") {
  cat("Parameters Used -> ", method_selected, "\t -> (", dmeth, " dmeth )\n" )
  hclust(as.dist(x), method = method_selected)
}

for (method_current in hclust_methods)
{
  # Debugging step 
  # method_current = hclust_methods[1]
  
  # Computing clusters
  cluster_genetics = hclustfunc(genetics_data_matrix_c39, method_selected = method_current)
  
  
  # Fix 2022.11.28 
  
  # Creating object
  create_object_command <- paste0( "strain_distrubution_distance_hclust_genetics_c39_", method_current, " <- cluster_genetics" )
  eval(parse(text = create_object_command))    
  
  # Saving object
  # Path to save
  path_to_save_object <- paste0("../R_Data/strain_distrubution_distance_hclust_genetics_c39_", method_current, ".RData") 
  # Saving process
  saving_process_command <- paste0( "save( strain_distrubution_distance_hclust_genetics_c39_", method_current, ", file = path_to_save_object )" )
  eval(parse(text = saving_process_command))    

  

  # Generating pdf output.
  pdf(paste("../Plots/part08_02_genetic_data_distance_multidimensional_scaling_plots_clusters_genetics_c39_", method_current, graph_suffix, ".pdf", sep = ""), height = 52, width = 40)
  # Defining the number of plots
  par(mfrow = c(1, 1))
  plot(cluster_genetics,
    # col = "darkorange",
    # col = color_01,
    lwd = 0.5,
    # pch = 16,
    # pch = shape_01,
    # pch = 17,
    # type = "p",
    pch = 19,
    # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
    # main = "genetics (Strain Distrubution Distance)",
    # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
    # xlim = c( 1.1 * min(x_values_genetics),
    #           max(x_values_genetics) * 1.1  ),
    # ylim = c( -0.5*max(cluster_genetics$height), max(cluster_genetics$height) ),
    # ylim = c(0, y_max_value_current * 1.2  ),
    # xlab = "Time",
    ann = FALSE,
    # xlab = "fdsfsd",
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
  mtext( paste0("Genetics (Strain Distribution Distance) - ", str_to_title(gsub( pattern = "\\.", replacement = " ", x = method_current ))),
    side = 3,
    line = -3.5,
    cex = 5,
    outer = TRUE
  )
  dev.off()
}


