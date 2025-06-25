# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.06.23. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen = 20)

set.seed(8675309)

# Specifying the number of cross-validation folds.
# Number of countries
no_of_countries <- 42
# Number of drops
no_of_drops <- 5

# Number of cross-validations 
no_fo_cv_folds <-1000

# Matrix to save indexes 
part11_01_cross_validation_samples_to_keep <- matrix( data = 0, nrow = no_fo_cv_folds, ncol = (no_of_countries - no_of_drops) )
dim(part11_01_cross_validation_samples_to_keep)


# Filling random subsets to keep in rows
for(i in c(1:no_fo_cv_folds))
{
  # Debugging
  # i <- 1
  
  # Saving i-th row
  part11_01_cross_validation_samples_to_keep[i,] <- sample(x = no_of_countries, size = (no_of_countries - no_of_drops), replace = FALSE)

  
# End of -> for(i in c(1:no_fo_cv_folds))   
}  


# Saving the setup
# Path
part11_01_cross_validation_samples_to_keep_path <- paste0("../R_Data/part11_01_cross_validation_samples_to_keep.RData") 
# Saving RData
save( part11_01_cross_validation_samples_to_keep, file = part11_01_cross_validation_samples_to_keep_path )


