# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.05.19. ask
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



# Library for permutations
library(gtools)


# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
# library(readr)

# To convert dates from text to dates
# library(lubridate)

# Reading data
# Incidence from RData file.
# load(file = paste("../R_Data/incidence_data_selected_fixed.RData"))
# Mortality as RData file.
# load(file = paste("../R_Data/mortality_data_selected_fixed.RData"))
load(file = paste("../R_Data/countries.RData"))
graph_suffix = "_eu"



# Population data
# https://population.un.org/wpp/Download/Standard/Population/
# Path for the data
# population_data_all_path = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"
population_data_all_path = "../Data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"
# Reading data
population_data_raw = data.frame(read_excel(path = population_data_all_path))[-c(1:11), ]
population_data_all = population_data_raw[-c(1), ]
names(population_data_all) = population_data_raw[c(1), ]
dim(population_data_all)
head(population_data_all)
# Copying country
population_data_all$Country = population_data_all$`Region, subregion, country or area *`

# Extracting countries we need.
# Countries to select
population_data_all$Country[which(population_data_all$Country == "Russian Federation")] = "Russia"
population_data_all$Country[which(population_data_all$Country == "Republic of Moldova")] = "Moldova"
which_rows_to_extract = which(population_data_all$Country %in% countries)
# Getting only the appropriate row and columns
columns_to_extract <- c( dim(population_data_all)[2] - 3, dim(population_data_all)[2] - 2, dim(population_data_all)[2] - 1, dim(population_data_all)[2])
population_data_selected = population_data_all[which_rows_to_extract, columns_to_extract ]
population_data_selected$Counts2018 = round(x = as.numeric(population_data_selected$`2018`), digits = 3) * 1000
population_data_selected$Counts2019 = round(x = as.numeric(population_data_selected$`2019`), digits = 3) * 1000
population_data_selected$Counts2020 = round(x = as.numeric(population_data_selected$`2020`), digits = 3) * 1000
# Sorting by country
population_data_selected <- population_data_selected[ order(population_data_selected$Country),  ]

# saving population data as RData file.
save(population_data_selected, file = paste("../R_Data/population_data_selected.RData"))






# Fix 2022.11.01

# Reading cardiovascular_disease_death_rates
cardiovascular_disease_death_rates_raw_path <-  paste("../Data/Our_world_in_data/cardiovascular-disease-death-rates_fixed.csv")
cardiovascular_disease_death_rates_raw <- read.table( file = cardiovascular_disease_death_rates_raw_path, sep = ",", header = TRUE)
dim(cardiovascular_disease_death_rates_raw)
summary(cardiovascular_disease_death_rates_raw$Year)
cardiovascular_disease_death_rates_raw2019 <- cardiovascular_disease_death_rates_raw[ cardiovascular_disease_death_rates_raw$Year == 2019, ]
names(cardiovascular_disease_death_rates_raw2019)[4] <- "cardiovascular_disease_death_rate_2019"

# Getting countries of interest 
sum( cardiovascular_disease_death_rates_raw2019$Entity %in% countries )
cardiovascular_disease_death_rates_eu2019 <- cardiovascular_disease_death_rates_raw2019[ cardiovascular_disease_death_rates_raw2019$Entity %in% countries, ]
dim(cardiovascular_disease_death_rates_eu2019)
length(countries)
head(cardiovascular_disease_death_rates_eu2019)
# saving population data as RData file.
save(cardiovascular_disease_death_rates_eu2019, file = paste("../R_Data/cardiovascular_disease_death_rates_eu2019.RData"))





# Reading diabetes-prevalence_fixed
diabetes_prevalence_fixed_raw_path <-  paste("../Data/Our_world_in_data/diabetes-prevalence_fixed.csv")
diabetes_prevalence_fixed_raw <- read.table( file = diabetes_prevalence_fixed_raw_path, sep = ",", header = TRUE)
dim(diabetes_prevalence_fixed_raw)
summary(diabetes_prevalence_fixed_raw$Year)
diabetes_prevalence_fixed_raw2019 <- diabetes_prevalence_fixed_raw[ diabetes_prevalence_fixed_raw$Year == 2019, ]
names(diabetes_prevalence_fixed_raw2019)[4] <- "diabetes_prevalence_2019"

# Getting countries of interest 
sum( diabetes_prevalence_fixed_raw2019$Entity %in% countries )
diabetes_prevalence_fixed_eu2019 <- diabetes_prevalence_fixed_raw2019[ diabetes_prevalence_fixed_raw2019$Entity %in% countries, ]
dim(diabetes_prevalence_fixed_eu2019)
length(countries)
head(diabetes_prevalence_fixed_eu2019)
# saving population data as RData file.
save(diabetes_prevalence_fixed_eu2019, file = paste("../R_Data/diabetes_prevalence_fixed_eu2019.RData"))





# Reading fertility-rate-complete-gapminder
fertility_rate_complete_gapminder_fixed_raw_path <-  paste("../Data/Our_world_in_data/fertility-rate-complete-gapminder_fixed.csv")
fertility_rate_complete_gapminder_fixed_raw <- read.table( file = fertility_rate_complete_gapminder_fixed_raw_path, sep = ",", header = TRUE)
dim(fertility_rate_complete_gapminder_fixed_raw)
summary(fertility_rate_complete_gapminder_fixed_raw$Year)
fertility_rate_complete_gapminder_fixed_raw2017 <- fertility_rate_complete_gapminder_fixed_raw[ fertility_rate_complete_gapminder_fixed_raw$Year == 2017, ]
names(fertility_rate_complete_gapminder_fixed_raw2017)[4] <- "total_fertility_rate_2017"

# Getting countries of interest 
sum( fertility_rate_complete_gapminder_fixed_raw2017$Entity %in% countries )
countries[which( !countries %in% fertility_rate_complete_gapminder_fixed_raw2017$Entity )]
# Fix 2022.11.02. Fixing Slovak Republic to Slovakia
fertility_rate_complete_gapminder_fixed_raw2017$Entity[ fertility_rate_complete_gapminder_fixed_raw2017$Entity == "Slovak Republic" ] <- "Slovakia"
sum( fertility_rate_complete_gapminder_fixed_raw2017$Entity %in% countries )
fertility_rate_complete_gapminder_fixed_eu2017 <- fertility_rate_complete_gapminder_fixed_raw2017[ fertility_rate_complete_gapminder_fixed_raw2017$Entity %in% countries, ]
dim(fertility_rate_complete_gapminder_fixed_eu2017)
length(countries)
head(fertility_rate_complete_gapminder_fixed_eu2017)
# saving population data as RData file.
save(fertility_rate_complete_gapminder_fixed_eu2017, file = paste("../R_Data/fertility_rate_complete_gapminder_fixed_eu2017.RData"))





# Reading gdp-per-capita-maddison_fixed
gdp_per_capita_maddison_fixed_raw_path <-  paste("../Data/Our_world_in_data/gdp-per-capita-maddison-2020_fixed.csv")
gdp_per_capita_maddison_fixed_raw <- read.table( file = gdp_per_capita_maddison_fixed_raw_path, sep = ",", header = TRUE)
dim(gdp_per_capita_maddison_fixed_raw)
summary(gdp_per_capita_maddison_fixed_raw$Year)
gdp_per_capita_maddison_fixed_raw2018 <- gdp_per_capita_maddison_fixed_raw[ gdp_per_capita_maddison_fixed_raw$Year == 2018, -dim(gdp_per_capita_maddison_fixed_raw)[2] ]
names(gdp_per_capita_maddison_fixed_raw2018)[4] <- "gdp_per_capita_2018"

# Getting countries of interest
sum( gdp_per_capita_maddison_fixed_raw2018$Entity %in% countries )
gdp_per_capita_maddison_fixed_eu2018 <- gdp_per_capita_maddison_fixed_raw2018[ gdp_per_capita_maddison_fixed_raw2018$Entity %in% countries, ]
dim(gdp_per_capita_maddison_fixed_eu2018)
length(countries)
# saving population data as RData file.
save(gdp_per_capita_maddison_fixed_eu2018, file = paste("../R_Data/gdp_per_capita_maddison_fixed_eu2018.RData"))





# Reading gross-domestic-product_fixed.csv
gross_domestic_product_fixed_raw_path <-  paste("../Data/Our_world_in_data/gross-domestic-product_fixed.csv")
gross_domestic_product_fixed_raw <- read.table( file = gross_domestic_product_fixed_raw_path, sep = ",", header = TRUE)
dim(gross_domestic_product_fixed_raw)
summary(gross_domestic_product_fixed_raw$Year)
gross_domestic_product_fixed_raw2019 <- gross_domestic_product_fixed_raw[ gross_domestic_product_fixed_raw$Year == 2019, ]
names(gross_domestic_product_fixed_raw2019)[4] <- "gdp_overall_2019"

# Getting countries of interest 
sum( gross_domestic_product_fixed_raw2019$Entity %in% countries )
gross_domestic_product_fixed_eu2019 <- gross_domestic_product_fixed_raw2019[ gross_domestic_product_fixed_raw2019$Entity %in% countries, ]
dim(gross_domestic_product_fixed_eu2019)
length(countries)
head(gross_domestic_product_fixed_eu2019)
# saving population data as RData file.
save(gross_domestic_product_fixed_eu2019, file = paste("../R_Data/gross_domestic_product_fixed_eu2019.RData"))





# Reading hospital-beds-per-1000-people_fixed
hospital_beds_per_1000_people_fixed_raw_path <-  paste("../Data/Our_world_in_data/hospital-beds-per-1000-people_fixed.csv")
hospital_beds_per_1000_people_fixed_raw <- read.table( file = hospital_beds_per_1000_people_fixed_raw_path, sep = ",", header = TRUE)
dim(hospital_beds_per_1000_people_fixed_raw)
summary(hospital_beds_per_1000_people_fixed_raw$Year)
hospital_beds_per_1000_people_fixed_raw2013 <- hospital_beds_per_1000_people_fixed_raw[ hospital_beds_per_1000_people_fixed_raw$Year == 2013, ]
names(hospital_beds_per_1000_people_fixed_raw2013)[4] <- "hospital_beds_per1000_2013"

# Getting countries of interest 
sum( hospital_beds_per_1000_people_fixed_raw2013$Entity %in% countries )
hospital_beds_per_1000_people_fixed_eu2013 <- hospital_beds_per_1000_people_fixed_raw2013[ hospital_beds_per_1000_people_fixed_raw2013$Entity %in% countries, ]
dim(hospital_beds_per_1000_people_fixed_eu2013)
length(countries)
head(hospital_beds_per_1000_people_fixed_eu2013)
# saving population data as RData file.
save(hospital_beds_per_1000_people_fixed_eu2013, file = paste("../R_Data/hospital_beds_per_1000_people_fixed_eu2013.RData"))





# Reading human-development-index_index
human_development_index_fixed_raw_path <-  paste("../Data/Our_world_in_data/human-development-index_fixed.csv")
human_development_index_fixed_raw <- read.table( file = human_development_index_fixed_raw_path, sep = ",", header = TRUE)
dim(human_development_index_fixed_raw)
summary(human_development_index_fixed_raw$Year)
human_development_index_fixed_raw2017 <- human_development_index_fixed_raw[ human_development_index_fixed_raw$Year == 2017, ]
names(human_development_index_fixed_raw2017)[4] <- "human_development_index_2017"

# Getting countries of interest 
sum( human_development_index_fixed_raw2017$Entity %in% countries )
human_development_index_fixed_eu2017 <- human_development_index_fixed_raw2017[ human_development_index_fixed_raw2017$Entity %in% countries, ]
dim(human_development_index_fixed_eu2017)
length(countries)
head(human_development_index_fixed_eu2017)
# saving population data as RData file.
save(human_development_index_fixed_eu2017, file = paste("../R_Data/human_development_index_fixed_eu2017.RData"))





# Reading life_expectancy_fixed
life_expectancy_fixed_raw_path <-  paste("../Data/Our_world_in_data/life-expectancy_fixed.csv")
life_expectancy_fixed_raw <- read.table( file = life_expectancy_fixed_raw_path, sep = ",", header = TRUE)
dim(life_expectancy_fixed_raw)
summary(life_expectancy_fixed_raw$Year)
life_expectancy_fixed_raw2019 <- life_expectancy_fixed_raw[ life_expectancy_fixed_raw$Year == 2019, ]
names(life_expectancy_fixed_raw2019)[4] <- "life_expectancy_2019"

# Getting countries of interest 
sum( life_expectancy_fixed_raw2019$Entity %in% countries )
life_expectancy_fixed_eu2019 <- life_expectancy_fixed_raw2019[ life_expectancy_fixed_raw2019$Entity %in% countries, ]
dim(life_expectancy_fixed_eu2019)
length(countries)
head(life_expectancy_fixed_eu2019)
# saving population data as RData file.
save(life_expectancy_fixed_eu2019, file = paste("../R_Data/life_expectancy_fixed_eu2019.RData"))





# Reading median-age_fixed
median_age_fixed_raw_path <-  paste("../Data/Our_world_in_data/median-age_fixed.csv")
median_age_fixed_raw <- read.table( file = median_age_fixed_raw_path, sep = ",", header = TRUE)
dim(median_age_fixed_raw)
summary(median_age_fixed_raw$Year)
median_age_fixed_raw2015 <- median_age_fixed_raw[ median_age_fixed_raw$Year == 2015, ]
names(median_age_fixed_raw2015)[4] <- "median_age_2015"

# Getting countries of interest 
sum( median_age_fixed_raw2015$Entity %in% countries )
median_age_fixed_eu2015 <- median_age_fixed_raw2015[ median_age_fixed_raw2015$Entity %in% countries, ]
dim(median_age_fixed_eu2015)
length(countries)
head(median_age_fixed_eu2015)
# saving population data as RData file.
save(median_age_fixed_eu2015, file = paste("../R_Data/median_age_fixed_eu2015.RData"))





# Reading population-density_fixed
population_density_fixed_raw_path <-  paste("../Data/Our_world_in_data/population-density_fixed.csv")
population_density_fixed_raw <- read.table( file = population_density_fixed_raw_path, sep = ",", header = TRUE)
dim(population_density_fixed_raw)
summary(population_density_fixed_raw$Year)
population_density_fixed_raw2018 <- population_density_fixed_raw[ population_density_fixed_raw$Year == 2018, ]
names(population_density_fixed_raw2018)[4] <- "population_density_2018"

# Getting countries of interest 
sum( population_density_fixed_raw2018$Entity %in% countries )
population_density_fixed_eu2018 <- population_density_fixed_raw2018[ population_density_fixed_raw2018$Entity %in% countries, ]
dim(population_density_fixed_eu2018)
length(countries)
head(population_density_fixed_eu2018)
# saving population data as RData file.
save(population_density_fixed_eu2018, file = paste("../R_Data/population_density_fixed_eu2018.RData"))





# Reading share-of-adults-defined-as-obese_fixed
share_of_adults_defined_as_obese_fixed_raw_path <-  paste("../Data/Our_world_in_data/share-of-adults-defined-as-obese_fixed.csv")
share_of_adults_defined_as_obese_fixed_raw <- read.table( file = share_of_adults_defined_as_obese_fixed_raw_path, sep = ",", header = TRUE)
dim(share_of_adults_defined_as_obese_fixed_raw)
summary(share_of_adults_defined_as_obese_fixed_raw$Year)
share_of_adults_defined_as_obese_fixed_raw2016 <- share_of_adults_defined_as_obese_fixed_raw[ share_of_adults_defined_as_obese_fixed_raw$Year == 2016, ]
names(share_of_adults_defined_as_obese_fixed_raw2016)[4] <- "share_of_obese_adults_2016"

# Getting countries of interest 
sum( share_of_adults_defined_as_obese_fixed_raw2016$Entity %in% countries )
share_of_adults_defined_as_obese_fixed_eu2016 <- share_of_adults_defined_as_obese_fixed_raw2016[ share_of_adults_defined_as_obese_fixed_raw2016$Entity %in% countries, ]
dim(share_of_adults_defined_as_obese_fixed_eu2016)
length(countries)
head(share_of_adults_defined_as_obese_fixed_eu2016)
# saving population data as RData file.
save(share_of_adults_defined_as_obese_fixed_eu2016, file = paste("../R_Data/share_of_adults_defined_as_obese_fixed_eu2016.RData"))





# Reading share-of-men-who-are-smoking_fixed
share_of_men_who_are_smoking_fixed_raw_path <-  paste("../Data/Our_world_in_data/share-of-men-who-are-smoking_fixed.csv")
share_of_men_who_are_smoking_fixed_raw <- read.table( file = share_of_men_who_are_smoking_fixed_raw_path, sep = ",", header = TRUE)
dim(share_of_men_who_are_smoking_fixed_raw)
summary(share_of_men_who_are_smoking_fixed_raw$Year)
share_of_men_who_are_smoking_fixed_raw2018 <- share_of_men_who_are_smoking_fixed_raw[ share_of_men_who_are_smoking_fixed_raw$Year == 2016, ]
names(share_of_men_who_are_smoking_fixed_raw2018)[4] <- "share_of_men_smoke_2018"

# Getting countries of interest 
sum(share_of_men_who_are_smoking_fixed_raw2018$Entity %in% countries )
countries[which( !countries %in% share_of_men_who_are_smoking_fixed_raw2018$Entity )]
share_of_men_who_are_smoking_fixed_eu2018 <- share_of_men_who_are_smoking_fixed_raw2018[ share_of_men_who_are_smoking_fixed_raw2018$Entity %in% countries, ]
dim(share_of_men_who_are_smoking_fixed_eu2018)
length(countries)
head(share_of_men_who_are_smoking_fixed_eu2018)
# saving population data as RData file.
save(share_of_men_who_are_smoking_fixed_eu2018, file = paste("../R_Data/share_of_men_who_are_smoking_fixed_eu2018.RData"))



# Reading share-of-population-in-extreme-poverty_fixed
share_of_population_in_extreme_poverty_fixed_raw_path <-  paste("../Data/Our_world_in_data/share-of-population-in-extreme-poverty_fixed.csv")
share_of_population_in_extreme_poverty_fixed_raw <- read.table( file = share_of_population_in_extreme_poverty_fixed_raw_path, sep = ",", header = TRUE)
dim(share_of_population_in_extreme_poverty_fixed_raw)
summary(share_of_population_in_extreme_poverty_fixed_raw$Year)
share_of_population_in_extreme_poverty_fixed_raw2019 <- share_of_population_in_extreme_poverty_fixed_raw[ share_of_population_in_extreme_poverty_fixed_raw$Year == 2019, ]
names(share_of_population_in_extreme_poverty_fixed_raw2019)[4] <- "share_of_extreme_poverty_population_2016"

# Getting countries of interest 
sum( share_of_population_in_extreme_poverty_fixed_raw2019$Entity %in% countries )
share_of_population_in_extreme_poverty_fixed_eu2019 <- share_of_population_in_extreme_poverty_fixed_raw2019[ share_of_population_in_extreme_poverty_fixed_raw2019$Entity %in% countries, ]
dim(share_of_population_in_extreme_poverty_fixed_eu2019)
length(countries)
head(share_of_population_in_extreme_poverty_fixed_eu2019)
# saving population data as RData file.
save(share_of_population_in_extreme_poverty_fixed_eu2019, file = paste("../R_Data/share_of_population_in_extreme_poverty_fixed_eu2019.RData"))



# Reading share-of-women-who-are-smoking_fixed
share_of_women_who_are_smoking_fixed_raw_path <- paste("../Data/Our_world_in_data/share-of-women-who-are-smoking_fixed.csv")
share_of_women_who_are_smoking_fixed_raw <- read.table( file = share_of_women_who_are_smoking_fixed_raw_path, sep = ",", header = TRUE)
dim(share_of_women_who_are_smoking_fixed_raw)
summary(share_of_women_who_are_smoking_fixed_raw$Year)
share_of_women_who_are_smoking_fixed_raw2018 <- share_of_women_who_are_smoking_fixed_raw[ share_of_women_who_are_smoking_fixed_raw$Year == 2018, ]
names(share_of_women_who_are_smoking_fixed_raw2018)[4] <- "share_of_women_smoke_2018"

# Getting countries of interest 
sum( share_of_women_who_are_smoking_fixed_raw2018$Entity %in% countries )
countries[which( !countries %in% share_of_women_who_are_smoking_fixed_raw2018$Entity )]
share_of_women_who_are_smoking_fixed_eu2018 <- share_of_women_who_are_smoking_fixed_raw2018[ share_of_women_who_are_smoking_fixed_raw2018$Entity %in% countries, ]
dim(share_of_women_who_are_smoking_fixed_eu2018)
length(countries)
which_countries <- which(!countries %in% share_of_women_who_are_smoking_fixed_eu2018$Entity)
share_of_women_who_are_smoking_fixed_eu2018$Entity[which_countries]
head(share_of_women_who_are_smoking_fixed_eu2018)
# saving population data as RData file.
save(share_of_women_who_are_smoking_fixed_eu2018, file = paste("../R_Data/share_of_women_who_are_smoking_fixed_eu2018.RData"))





# Multiple merge
# Code from -> https://www.statology.org/merge-multiple-data-frames-in-r/
# df_list <- list(df1, df2, df3)      
# merge all data frames together
# Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  



names(cardiovascular_disease_death_rates_eu2019)
names(diabetes_prevalence_fixed_eu2019)
names(fertility_rate_complete_gapminder_fixed_eu2017)
names(gdp_per_capita_maddison_fixed_eu2018)
names(gross_domestic_product_fixed_eu2019)
names(hospital_beds_per_1000_people_fixed_eu2013)
names(human_development_index_fixed_eu2017)
names(life_expectancy_fixed_eu2019)
names(median_age_fixed_eu2015)
names(population_density_fixed_eu2018)
names(share_of_adults_defined_as_obese_fixed_eu2016)
names(share_of_men_who_are_smoking_fixed_eu2018)
names(share_of_population_in_extreme_poverty_fixed_eu2019)
names(share_of_women_who_are_smoking_fixed_eu2018)





# list to merge
df_list <- list(cardiovascular_disease_death_rate_2019    = cardiovascular_disease_death_rates_eu2019,
                diabetes_prevalence_2019                  = diabetes_prevalence_fixed_eu2019,
                total_fertility_rate_2017                 = fertility_rate_complete_gapminder_fixed_eu2017,
                gdp_per_capita_2018                       = gdp_per_capita_maddison_fixed_eu2018,
                # gross_domestic_product_fixed_eu2019     = gross_domestic_product_fixed_eu2019,
                hospital_beds_per1000_2013                = hospital_beds_per_1000_people_fixed_eu2013,
                human_development_index_2017              = human_development_index_fixed_eu2017,
                life_expectancy_2019                      = life_expectancy_fixed_eu2019,
                median_age_2015                           = median_age_fixed_eu2015,
                population_density_2018                   = population_density_fixed_eu2018,
                share_of_obese_adults_2016                = share_of_adults_defined_as_obese_fixed_eu2016,
                # share_of_men_smoke_2018                 = share_of_men_who_are_smoking_fixed_eu2018,
                share_of_extreme_poverty_population_2016  = share_of_population_in_extreme_poverty_fixed_eu2019
                # share_of_women_smoke_2018               = share_of_women_who_are_smoking_fixed_eu2018
                ) 



auxilliary_data_raw_merge <- Reduce(function(x, y) merge(x, y, by.x = "Entity", by.y = "Entity"), df_list)  
names(auxilliary_data_raw_merge)
dim(auxilliary_data_raw_merge)

cols_to_keep <- c("Entity", "Code.x", names(df_list))
auxilliary_data_merge <- auxilliary_data_raw_merge[, cols_to_keep ]
dim(auxilliary_data_merge)
names(auxilliary_data_merge)
names(auxilliary_data_merge)[c(1,2)] <-  c("Country", "Country_Code" )
head(auxilliary_data_merge)

# Saving the data
save(auxilliary_data_merge, file = paste("../R_Data/auxilliary_data_merge.RData"))


# Creating standardized analog

# divding by column means using apply()
mean_subtracted <-  t( apply( auxilliary_data_merge[,-c(1,2)], 1, function(x) x - colMeans(auxilliary_data_merge[,-c(1,2)])) )
mean_subtracted_divided_sds <- t( apply( mean_subtracted, 1, function(x) x/colSds(mean_subtracted)  ) )

# Debugging and checking
# auxilliary_data_merge[,3] - mean(auxilliary_data_merge[,3])
# mean_subtracted[,1]
# mean_subtracted[,1]/colSds(mean_subtracted)[1]
# mean_subtracted_divided_sds[,1]


# Processing matrix
auxilliary_data_merge_standardized <- mean_subtracted_divided_sds
rownames(auxilliary_data_merge_standardized) <- auxilliary_data_merge$Country



# Saving the data
save(auxilliary_data_merge_standardized, file = paste("../R_Data/auxilliary_data_merge_standardized.RData"))


