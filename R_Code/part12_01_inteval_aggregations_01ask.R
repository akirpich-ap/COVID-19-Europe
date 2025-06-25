# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.06.23. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen = 20)

# set.seed(8675309)

# Specifying the number of cross-validation folds.
# Number of countries
no_of_countries <- 42
# Number of drops


# Number of weeks to aggregated
part12_01_inteval_aggregations <- c(1,2,3,4,5)


# Saving the setup
# Path
part12_01_inteval_aggregations_path <- paste0("../R_Data/part12_01_inteval_aggregations.RData") 
# Saving RData
save( part12_01_inteval_aggregations, file = part12_01_inteval_aggregations_path )


