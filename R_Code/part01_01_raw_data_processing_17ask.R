# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.91. ask
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

# Library for read_csv
library(readr)

# To convert dates from text to dates
library(lubridate)



# Setting the correct working directory.




# Path for the data
# COVID-19 data from https://github.com/CSSEGISandData/COVID-19
# Files relative
incidence_data_all_path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
mortality_data_all_path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"




# Reading data


# incidence
# Raw files read
incidence_lines_all_raw = readLines(incidence_data_all_path)
length(incidence_lines_all_raw)

# Fixing "Cote d 'Ivoire" to "Cote dIvoire" to ensure proper reading
incidence_lines_all_raw[100] = gsub(pattern = "Cote d\\'Ivoire", replacement = "Cote dIvoire", x = incidence_lines_all_raw[100])

# Reading in parts
# Part 0
incidence_data_all_raw_part00 = as.vector(data.frame(read.table(text = incidence_lines_all_raw[1], sep = ",")))
length(incidence_data_all_raw_part00)
incidence_data_all_raw = data.frame(read.table(text = incidence_lines_all_raw[-1], sep = ","))
dim(incidence_data_all_raw)
# Combining
names(incidence_data_all_raw) = incidence_data_all_raw_part00
names(incidence_data_all_raw)
dim(incidence_data_all_raw)

# Fix 2022.04.05.
# Subsetting up to Feb 15
which_incidence_column_021522 <- which( names(incidence_data_all_raw) == "2/15/22" )
incidence_data_all_raw <- incidence_data_all_raw[, c(1:which_incidence_column_021522)]
names(incidence_data_all_raw)




# mortality
# Raw files read
mortality_lines_all_raw = readLines(mortality_data_all_path)
length(mortality_lines_all_raw)

# Fixing "Cote d 'Ivoire" to "Cote dIvoire" to ensure proper reading
mortality_lines_all_raw[100] = gsub(pattern = "Cote d\\'Ivoire", replacement = "Cote dIvoire", x = mortality_lines_all_raw[100])


# Reading in parts
# Part 0
mortality_data_all_raw_part00 = as.vector(data.frame(read.table(text = mortality_lines_all_raw[1], sep = ",")))
length(mortality_data_all_raw_part00)
mortality_data_all_raw = data.frame(read.table(text = mortality_lines_all_raw[-1], sep = ","))
dim(mortality_data_all_raw)
# Combining
names(mortality_data_all_raw) = mortality_data_all_raw_part00
names(mortality_data_all_raw)
dim(mortality_data_all_raw)

# Fix 2022.11.04.
# Subsetting up to Feb 15, 2022 due to war
which_mortality_column_021522 <- which( names(mortality_data_all_raw) == "2/15/22" )
mortality_data_all_raw <- mortality_data_all_raw[, c(1:which_mortality_column_021522)]
names(mortality_data_all_raw)





# Extracting countries we need.
# Countries to select
# Belarus neighbours
# countries_to_select = c("Belarus", "Estonia", "Latvia", "Lithuania", "Poland", "Russia", "Ukraine")
# Europe
# countries_to_select = c("Albania", "Andorra", "Armenia", "Austria", "Belarus", "Belgium", 
# "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", 
# "France", "Germany", "Greece", "Holy See", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
# "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", 
# "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", 
# "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
# Europe w/o microstates

# countries_to_select = c("Albania", "Armenia", "Austria", "Belarus", "Belgium", 
# "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", 
# "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
# "Lithuania", "Luxembourg", "Moldova", "Montenegro", 
# "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", 
# "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")

countries_to_select = c("Albania", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                        "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", 
                        "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                        "Malta", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Moldova", 
                        "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", 
                        "Ukraine", "United Kingdom")

# All
# countries_to_select = NULL
# european_countreies_to_select = 


# incidence
if (length(countries_to_select) != 0) {
  incidence_data_selected_raw = incidence_data_all_raw[which(incidence_data_all_raw$`Country/Region` %in% countries_to_select & mortality_data_all_raw$`Province/State`==""),]
} else {
  incidence_data_selected_raw = incidence_data_all_raw[which(mortality_data_all_raw$`Province/State`==""),]
}
countries = unique(incidence_data_selected_raw$`Country/Region`)
stopifnot(length(countries) == length(incidence_data_selected_raw[,"Country/Region"]))
incidence_data_selected_transposed = as.matrix(t(incidence_data_selected_raw))
incidence_data_selected = data.frame(incidence_data_selected_transposed[-c(1:4),])
names(incidence_data_selected) = as.vector(countries)
dim(incidence_data_selected)
# Fixing column names
head(incidence_data_selected)
# Fixing dates (using library(lubridate) )
head(incidence_data_selected)
tail(incidence_data_selected)
dim(incidence_data_selected)
row_count = dim(incidence_data_selected)[1]

incidence_data_selected_fixed = data.frame(matrix(ncol = length(countries), nrow = row_count))
names(incidence_data_selected_fixed) = as.vector(countries)
for (country in countries)
{
  # Debugging
  # country <- countries[1]
  # cat(country, "\n")
  
  temp = as.numeric(incidence_data_selected[,country])
  temp_diff = diff(temp)
  for (i in 1:(row_count-1))
  {
    if ( (temp_diff[i] < 0) && (i+1 < length(temp_diff)) )
    {
      # Getting next non negative index for interpolation
      temp_diff_truncate = temp_diff[(i+1):length(temp_diff)]
      second_index  = i + min( which( temp_diff[(i+1):length(temp_diff)] >=0 ) )
      
      # Interpolation object
      interpolation_object <- 
        approx( x = mdy(rownames(incidence_data_selected))[c(i-1,second_index)],
                y = temp_diff[c(i-1,second_index)], xout = mdy(rownames(incidence_data_selected))[c((i-1):second_index)] )  
      
      # Extracting values we need
      interpolation_object_x <- interpolation_object$x
      interpolation_object_y <- interpolation_object$y
      
      # Extra check
      # sum( !mdy(rownames(incidence_data_selected))[c((i-1):second_index)] == interpolation_object_x )
      # Saving interpolated values
      temp_diff[c((i-1):second_index)] <- interpolation_object_y
      # cat(i, "\n")
    }
    
    if ( (temp_diff[i] < 0) && (i+1 == length(temp_diff)) )
    {
      temp_diff[i+1] <- temp_diff[i+1]
      # cat(i, "\n")
    }
  }
  incidence_data_selected_fixed[,country] = c(0, temp_diff)
}

incidence_data_selected_fixed$Date = mdy(rownames(incidence_data_selected))
rownames(incidence_data_selected_fixed)
# Saving the processed results as RData file.
save(incidence_data_selected_fixed, file = paste("../R_Data/incidence_data_selected_fixed.RData"))
save(countries, file = paste("../R_Data/countries.RData"))



# mortality
# Select only countries we need and drop oversea regions (if include china and other countries some more manual selection will be needed)
if (length(countries_to_select) != 0) {
  mortality_data_selected_raw = mortality_data_all_raw[which(mortality_data_all_raw$`Country/Region` %in% countries_to_select & mortality_data_all_raw$`Province/State`==""),]
} else {
  mortality_data_selected_raw = mortality_data_all_raw[which(mortality_data_all_raw$`Province/State`==""),]
}
# Check for duplicates
stopifnot(length(countries) == length(mortality_data_selected_raw[,"Country/Region"]))
mortality_data_selected_transposed = as.matrix(t(mortality_data_selected_raw))
mortality_data_selected = data.frame(mortality_data_selected_transposed[-c(1:4),])
dim(mortality_data_selected)
# Fixing column names
names(mortality_data_selected) = as.vector(mortality_data_selected_transposed[2,])
head(mortality_data_selected)
# Fixing dates (using library(lubridate) )
mortality_data_selected$Date = mdy(rownames(mortality_data_selected))
head(mortality_data_selected)
tail(mortality_data_selected)
dim(mortality_data_selected)
row_count = dim(mortality_data_selected)[1]

mortality_data_selected_fixed = data.frame(matrix(ncol = length(countries), nrow = row_count))
names(mortality_data_selected_fixed) = as.vector(countries)
for (country in countries)
{
  # Debugging
  # country <- countries[1]
  # cat(country, "\n")
  
  temp = as.numeric(mortality_data_selected[,country])
  temp_diff = diff(temp)
  for (i in 1:(row_count-1))
  {
    if ( (temp_diff[i] < 0) && (i+1 < length(temp_diff)) )
    {
      # Getting next non negative index for interpolation
      temp_diff_truncate = temp_diff[(i+1):length(temp_diff)]
      second_index  = i + min( which( temp_diff[(i+1):length(temp_diff)] >=0 ) )
      
      # Interpolation object
      interpolation_object <- 
        approx( x = mdy(rownames(incidence_data_selected))[c(i-1,second_index)],
                y = temp_diff[c(i-1,second_index)], xout = mdy(rownames(incidence_data_selected))[c((i-1):second_index)] )  
      
      # Extracting values we need
      interpolation_object_x <- interpolation_object$x
      interpolation_object_y <- interpolation_object$y
      
      # Extra check
      # sum( !mdy(rownames(incidence_data_selected))[c((i-1):second_index)] == interpolation_object_x )
      # Saving interpolated values
      temp_diff[c((i-1):second_index)] <- interpolation_object_y
      # cat(i, "\n")
    }
    
    if ( (temp_diff[i] < 0) && (i+1 == length(temp_diff)) )
    {
      temp_diff[i+1] <- temp_diff[i+1]
      # cat(i, "\n")
    }
  }
  mortality_data_selected_fixed[,country] = c(0, temp_diff)
}

mortality_data_selected_fixed$Date = mdy(rownames(mortality_data_selected))
# Saving the processed results as RData file.
save(mortality_data_selected_fixed, file = paste("../R_Data/mortality_data_selected_fixed.RData"))