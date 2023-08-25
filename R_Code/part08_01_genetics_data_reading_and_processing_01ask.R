# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.13. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations.
library(matrixStats)

# Library for the latex exports in the nice format.
library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
library(readr)

# To convert dates from text to dates
library(lubridate)



# Loading countries names from part05_01 file
load(file = paste("../R_Data/travel_meta_countries.RData"))
# Full list
load(file = paste("../R_Data/countries.RData"))
# Reduced list withouth Serbia and Montenegro
load(file = paste("../R_Data/countries_travel.RData"))



# Path for the genetics data matrix
genetics_data_matrix_path <- paste("../Data/strains.csv")

# Reading data
genetics_data_matrix_raw <- read.table(file  = genetics_data_matrix_path, header = FALSE, sep = ",")
dim(genetics_data_matrix_raw)
head(genetics_data_matrix_raw)
# Fixing row and column names
colnames(genetics_data_matrix_raw) <- c( countries_travel, "Montenegro", "Serbia") 
rownames(genetics_data_matrix_raw) <- colnames(genetics_data_matrix_raw)
head(genetics_data_matrix_raw)

# Ordering columns alphabetically
genetics_data_matrix_column_ordered <- genetics_data_matrix_raw[, order(colnames(genetics_data_matrix_raw)) ]
# Ordering rows alphabetically
genetics_data_matrix_ordered_c39    <- as.matrix(genetics_data_matrix_column_ordered[ order(rownames(genetics_data_matrix_raw)),  ])

# Extra checks that what was done is correct.
isSymmetric(genetics_data_matrix_ordered_c39)
diag(genetics_data_matrix_ordered_c39)

genetics_data_matrix_ordered_c37 <- genetics_data_matrix_ordered_c39[ !(rownames(genetics_data_matrix_ordered_c39) %in% c("Montenegro", "Serbia")),
                                                                      !(colnames(genetics_data_matrix_ordered_c39) %in% c("Montenegro", "Serbia"))]
dim(genetics_data_matrix_ordered_c37)
# Checking 
sum( !genetics_data_matrix_raw[-c(dim(genetics_data_matrix_raw)[1], dim(genetics_data_matrix_raw)[1]-1),
                               -c(dim(genetics_data_matrix_raw)[2], dim(genetics_data_matrix_raw)[2]-1)] == genetics_data_matrix_ordered_c37)
sum( !genetics_data_matrix_raw[c(dim(genetics_data_matrix_raw)[1], dim(genetics_data_matrix_raw)[1]-1),
                               c(dim(genetics_data_matrix_raw)[2], dim(genetics_data_matrix_raw)[2]-1)] == 
                                 genetics_data_matrix_ordered_c39[ (rownames(genetics_data_matrix_ordered_c39) %in% c("Serbia", "Montenegro")),
                                                                   (colnames(genetics_data_matrix_ordered_c39) %in% c("Serbia", "Montenegro"))] )  


# Saving the data
save(genetics_data_matrix_ordered_c37, file = paste("../R_Data/genetics_data_matrix_ordered_c37.RData"))
save(genetics_data_matrix_ordered_c39, file = paste("../R_Data/genetics_data_matrix_ordered_c39.RData"))


