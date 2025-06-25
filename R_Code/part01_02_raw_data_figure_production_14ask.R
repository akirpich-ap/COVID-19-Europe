# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.07. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

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

# Reading data
# Incidence from RData file.
load(file = paste("../R_Data/incidence_data_selected_fixed.RData"))
# Mortality as RData file.
load(file = paste("../R_Data/mortality_data_selected_fixed.RData"))
load(file = paste("../R_Data/countries.RData"))
graph_suffix = "_eu"


# Fix 2023.06.01.
# Specifying the aggregation period before standardization.
aggregation_periods_in_days <- 7
# Standartization type where "p" =  population and "t" - tests
standartization_type <- "p"
# Rates per counts
rates_per_counts <- 100000
# Subsetting up to Feb 15
subsetting_end_date <- as.Date(x = "2022-02-15", origin = "1970-01-01" ) 




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



# Fix 2023.06.01 ask
# Reading the number of tests
# https://ourworldindata.org/coronavirus-testing
# https://github.com/owid/covid-19-data/tree/master/public/data/testing
# https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv
testing_data_all_path = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv"
# Reading data
testing_data_raw = read.csv(file = testing_data_all_path)
dim(testing_data_raw)
head(testing_data_raw)

# Countries meta data
travel_meta_countries_path <- "../Data/Travel/KCMD_DDH_meta_KCMD-EUI GMP_ Estimated trips.xlsx"
# Reading data
travel_meta_countries_raw <- data.frame(read_excel(path = travel_meta_countries_path))
dim(travel_meta_countries_raw)
# Fixing Bosnia and Herzegovina and Czechia
# sort(travel_meta_countries_raw$country)
travel_meta_countries_raw$country[ travel_meta_countries_raw$country == "Bosniaand Herzegovina" ] <- "Bosnia and Herzegovina"
travel_meta_countries_raw$country[ travel_meta_countries_raw$country == "Czech Republic" ]        <- "Czechia"
# Getting the list we need.
travel_meta_short <- data.frame(travel_meta_countries_raw[ travel_meta_countries_raw$country %in% countries, ])
travel_meta_long  <- rbind(travel_meta_short, c("SRB", "Serbia"),  c("MNE", "Montenegro") )
travel_meta_countries <- travel_meta_long[order(travel_meta_long$country), ] 
dim(travel_meta_countries)
# Saving
save(travel_meta_countries, file = paste("../R_Data/travel_meta_countries.RData"))


# Extracting testing rates for the countries of interest among available.
testing_data_raw_subset = testing_data_raw[ testing_data_raw$ISO.code %in% travel_meta_countries$code, ]
dim(testing_data_raw_subset)
names(testing_data_raw_subset)
head(testing_data_raw_subset)
length(unique(testing_data_raw_subset$ISO.code))

setdiff( unique(testing_data_raw_subset$ISO.code), travel_meta_countries$code  )
setdiff( travel_meta_countries$code, unique(testing_data_raw_subset$ISO.code)  )

testing_data_raw_subset_merged <- merge( x = testing_data_raw_subset, y = travel_meta_countries, by.x = "ISO.code", by.y = "code", sort = FALSE) 
names(testing_data_raw_subset_merged)
testing_data_merged <- testing_data_raw_subset_merged[ , c("Date", "Cumulative.total", "Daily.change.in.cumulative.total", "Cumulative.total.per.thousand",                
                                                        "Daily.change.in.cumulative.total.per.thousand", "X7.day.smoothed.daily.change", 
                                                        "X7.day.smoothed.daily.change.per.thousand", "Short.term.positive.rate", "Short.term.tests.per.case", "country") ]
# Converting dates to numeric
testing_data_merged$Date <- as.Date(testing_data_merged$Date) 
# Keeping up to subsetting_end_date 
testing_data_merged <- testing_data_merged[ testing_data_merged$Date <= subsetting_end_date, ] 
head(testing_data_merged)

# Saving testing_data_merged.
save(testing_data_merged, file = paste("../R_Data/testing_data_merged.RData"))


# Population for the studied countries.
# Extracting countries we need.
# Countries to select
population_data_all$Country[which(population_data_all$Country == "Russian Federation")] = "Russia"
population_data_all$Country[which(population_data_all$Country == "Republic of Moldova")] = "Moldova"
which_rows_to_extract = which(population_data_all$Country %in% countries)
# Getting only the appropriate row and columns
population_data_selected = population_data_all[which_rows_to_extract, c(dim(population_data_all)[2] - 1, dim(population_data_all)[2])]
population_data_selected$Counts2020 = round(x = as.numeric(population_data_selected$`2020`), digits = 3) * 1000
dim(population_data_selected)

# Fix 2022.11.16.
# Saving the country's population data.
save(population_data_selected, file = paste("../R_Data/population_data_selected.RData"))



# Fix 2023.06.01.
# Aggregation dates using -aggregation_periods_in_days increment from the last dates
dates_aggregation_incidence <- sort(seq(from = max(incidence_data_selected_fixed$Date), min(incidence_data_selected_fixed$Date)-aggregation_periods_in_days, by = - aggregation_periods_in_days))
dates_aggregation_mortality <- sort(seq(from = max(mortality_data_selected_fixed$Date), min(mortality_data_selected_fixed$Date)-aggregation_periods_in_days, by = - aggregation_periods_in_days))
sum( !dates_aggregation_incidence == dates_aggregation_mortality ) 
dates_aggregation <- dates_aggregation_incidence
# Saving
save(dates_aggregation, file = paste("../R_Data/dates_aggregation.RData"))


# Creating frames to fill
matrix_placeholder <- matrix(0, nrow  = length(dates_aggregation), ncol = length(countries))
incidence_data_aggregated <- data.frame( Date = dates_aggregation,   matrix_placeholder )
mortality_data_aggregated <- data.frame( Date = dates_aggregation,   matrix_placeholder )
testing_data_aggregated   <- data.frame( Date = dates_aggregation,   matrix_placeholder )

names(incidence_data_aggregated)[-1] <- countries
names(mortality_data_aggregated)[-1] <- countries
names(testing_data_aggregated)[-1] <- countries



# Aggregation loop
for (country_current in countries)
{
  # Debugging
  # country_current <- countries[1]
   
  for (date_current in dates_aggregation)
  {
    # date_current <- dates_aggregation[1]
    # date_current <- dates_aggregation[2]
    # date_current <- dates_aggregation[length(dates_aggregation)]

    # incidence
    # Indexes extraction
    which_dates_current_incidence <- incidence_data_selected_fixed$Date %in% c((date_current-aggregation_periods_in_days + 1):date_current)
    # Saving the sum
    incidence_data_aggregated[(incidence_data_aggregated$Date == date_current),country_current] <- 
      sum(incidence_data_selected_fixed[ which_dates_current_incidence, country_current]) 

    # mortality
    # Indexes extraction
    which_dates_current_mortality <- mortality_data_selected_fixed$Date %in% c((date_current-aggregation_periods_in_days + 1):date_current)
    # Saving the sum
    mortality_data_aggregated[(mortality_data_aggregated$Date == date_current),country_current] <- 
      sum(mortality_data_selected_fixed[ which_dates_current_mortality, country_current]) 
    
    # testing
    # Indexes extraction
    which_dates_current_testing <- which( (testing_data_merged$Date == date_current) * (testing_data_merged$country == country_current) == 1 )
    
    # Checking if missing the corresponding date in the dataset or data are NA.
    # In either of those cases the number of test is equal to incidence.
    if ( identical(which_dates_current_testing, integer(0)) || is.na(testing_data_merged[which_dates_current_testing, "X7.day.smoothed.daily.change" ]) )
    {  
      # Saving the sum of the incidence
      testing_data_aggregated[(testing_data_aggregated$Date == date_current),country_current] <-  -1
        # sum(incidence_data_selected_fixed[ which_dates_current_incidence, country_current]) 

    } else {
      # Saving the seven day average multiplied by 7 to account for the week totals.
      testing_data_aggregated[(testing_data_aggregated$Date == date_current),country_current] <- 
        7 * testing_data_merged[ which_dates_current_testing, "X7.day.smoothed.daily.change" ] 
    }
    
  # End of -> for (current_country in countries)   
  }  
  
  
# End of -> for (current_country in countries)
}  
save(incidence_data_aggregated, file = paste("../R_Data/incidence_data_aggregated.RData"))
save(mortality_data_aggregated, file = paste("../R_Data/mortality_data_aggregated.RData"))
save(testing_data_aggregated,   file = paste("../R_Data/testing_data_aggregated.RData"))




# Fix 2023.06.01
# Fixing and standardizing


# If the decision is to standardize by population
# Rates are computed using rates_per_counts constant from the beginning of the code. 
if (standartization_type == "p")
{
  # Creating datasets for standardization
  incidence_data_standardized <- incidence_data_aggregated
  mortality_data_standardized <- mortality_data_aggregated
  
  
  for (country in countries)
  {
    # Debugging step
    # current_country = countries_to_select[1]
    
    # current_country_population
    country_population = population_data_selected$Counts2020[which(population_data_selected$Country == country)]
    incidence_data_standardized[,country] = rates_per_counts * incidence_data_aggregated[,country] / country_population
    mortality_data_standardized[,country] = rates_per_counts * mortality_data_aggregated[,country] / country_population
    
    # End of -> for ( current_country in countries_to_select )
  }

  
# End of -> if (standartization_type <- "p")
}


# If the decision is to standardize by tests
# Rates are computed using rates_per_counts constant from the beginning of the code. 
if (standartization_type == "t")
{
  
  # Removing "Azerbaijan" and "Montenegro" sine limited or no info are aviable.
  countries <- setdiff( countries, c("Azerbaijan", "Montenegro") )
  # countries <- setdiff( countries, c("Montenegro") )
  # Saving and overwriting countries list in this case
  save(countries, file = paste("../R_Data/countries.RData"))
  
  
  # Subsetting only the current countries are available.
  incidence_data_aggregated <- incidence_data_aggregated[ , c(TRUE, names(incidence_data_aggregated)[-1] %in% countries) ]
  mortality_data_aggregated <- mortality_data_aggregated[ , c(TRUE, names(mortality_data_aggregated)[-1] %in% countries) ]
  testing_data_aggregated   <- testing_data_aggregated[ , c(TRUE, names(testing_data_aggregated)[-1] %in% countries) ]
  
  
  
  # Fix 2023.06.07.
  # Checking how much time in the beginning of the series to drop.
  list_of_dates <- NULL
  
  for(current_date in testing_data_aggregated$Date)
  {
    # Debugging
    # current_date <- testing_data_aggregated$Date[50]
    testing_data_aggregated_subset <- testing_data_aggregated[ testing_data_aggregated$Date == current_date , ]

    # Debugging
    cat("date=", as.Date(current_date, origin = "1970-01-01"), "(sum(testing_data_aggregated_subset[,-1 ]<0) = ", sum(testing_data_aggregated_subset[,-1 ]<0), "\n")
    if (sum(testing_data_aggregated_subset[,-1 ]<0) == 0) 
    {
      list_of_dates <- c(list_of_dates, current_date)
      
    # End of -> if (sum(testing_data_aggregated_subset[,-1 ]<0) > 0)  
    }  

  # End of -> for(current_date in testing_data_aggregated$Date)  
  }  
  # Extra check
  cat("length(list_of_dates) = ", length(list_of_dates), 
      "length(list_of_dates)/length(testing_data_aggregated$Date)", length(list_of_dates)/length(testing_data_aggregated$Date), "\n")
  
  
  # Subsetting only the dates where tests are available.
  incidence_data_aggregated <- incidence_data_aggregated[ incidence_data_aggregated$Date %in% list_of_dates, ]
  mortality_data_aggregated <- mortality_data_aggregated[ mortality_data_aggregated$Date %in% list_of_dates, ]
  testing_data_aggregated   <- testing_data_aggregated[ testing_data_aggregated$Date %in% list_of_dates, ]
  
  # Creating datasets for standardization
  incidence_data_standardized <- incidence_data_aggregated
  mortality_data_standardized <- mortality_data_aggregated
  
    
  for (country in countries)
  {
    # Debugging step
    # current_country = countries_to_select[1]
    
    # If denominator is 0 we make it equal to 1 to avoid division by zero.
    testing_data_aggregated_current_country <- testing_data_aggregated[,country]
    # Computing
    incidence_data_standardized[,country] = rates_per_counts * incidence_data_aggregated[,country]/testing_data_aggregated_current_country
    mortality_data_standardized[,country] = rates_per_counts * mortality_data_aggregated[,country]/testing_data_aggregated_current_country
    
    # End of -> for ( current_country in countries_to_select )
  }
  
  
  # End of -> if (standartization_type <- "p")
}








# Fix 2021.12.22
# incidence
# Creating standardized version to save
incidence_data_selected_fixed_plus_standardized = incidence_data_standardized
# Saving the processed results as RData file.
save(incidence_data_selected_fixed_plus_standardized, file = paste("../R_Data/incidence_data_standardized.RData"))
# mortality
# Creating standardized version to save
mortality_data_selected_fixed_plus_standardized = mortality_data_standardized
# Saving the processed results as RData file.
save(mortality_data_selected_fixed_plus_standardized, file = paste("../R_Data/mortality_data_standardized.RData"))




# Working with incidence

# Exploratory histograms
# hist(incidence_data_selected_fixed$Belarus_incidence_standardized)
# hist(incidence_data_selected_fixed$Latvia_incidence_standardized)
# hist(incidence_data_selected_fixed$Lithuania_incidence_standardized)
# hist(incidence_data_selected_fixed$Poland_incidence_standardized)
# hist(incidence_data_selected_fixed$Russia_incidence_standardized)
# hist(incidence_data_selected_fixed$Ukraine_incidence_standardized)

# Exploratory summaries
standardized_incidence_summaries = summary(incidence_data_standardized[,countries[1]])
for (country in countries[2:length(countries)])
{
  standardized_incidence_summaries = rbind(standardized_incidence_summaries,
  summary(incidence_data_standardized[,country]))
}

standardized_incidence_summaries_extended = data.frame(Country = countries, standardized_incidence_summaries)
standardized_incidence_summaries_extended


# Getting min and max valued for the future histogram boundaries.
incidence_overall_min = range(as.vector(standardized_incidence_summaries))[1]
incidence_overall_max = range(as.vector(standardized_incidence_summaries))[2]
# Defining bin cutoffs
incidence_overall_bins = c(0:25) * incidence_overall_max / 25


# CSV table for data
standardized_incidence_summaries_extended_csv_path = paste0("../R_Output/standardized_incidence_summaries_extended.csv")
write.table(standardized_incidence_summaries_extended, file = standardized_incidence_summaries_extended_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
# standardized_incidence_summaries_extended_csv_nonames_path = paste0("R_Output/standardized_incidence_summaries_extended_nonames.csv")
# write.table( standardized_incidence_summaries_extended, file = standardized_incidence_summaries_extended_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)





# Working with mortality

# Exploratory histograms
# hist(mortality_data_selected_fixed$Belarus_mortality_standardized)
# hist(mortality_data_selected_fixed$Latvia_mortality_standardized)
# hist(mortality_data_selected_fixed$Lithuania_mortality_standardized)
# hist(mortality_data_selected_fixed$Poland_mortality_standardized)
# hist(mortality_data_selected_fixed$Russia_mortality_standardized)
# hist(mortality_data_selected_fixed$Ukraine_mortality_standardized)

# Exploratory summaries
standardized_mortality_summaries = summary(mortality_data_standardized[,countries[1]])
for (country in countries[2:length(countries)])
{
  standardized_mortality_summaries = rbind(standardized_mortality_summaries,
  summary(mortality_data_standardized[,country]))
}

standardized_mortality_summaries_extended = data.frame(Country = countries, standardized_mortality_summaries)
standardized_mortality_summaries_extended


# Getting min and max valued for the future histogram boundaries.
mortality_overall_min = range(as.vector(standardized_mortality_summaries))[1]
mortality_overall_max = range(as.vector(standardized_mortality_summaries))[2]
# Defining bin cutoffs
mortality_overall_bins = c(0:25) * mortality_overall_max / 25


# CSV table for data
standardized_mortality_summaries_extended_csv_path = paste0("../R_Output/standardized_mortality_summaries_extended.csv")
write.table(standardized_mortality_summaries_extended, file = standardized_mortality_summaries_extended_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
# standardized_mortality_summaries_extended_csv_nonames_path = paste0("R_Output/standardized_mortality_summaries_extended_nonames.csv")
# write.table( standardized_mortality_summaries_extended, file = standardized_mortality_summaries_extended_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)




# incidence histograms

# This for loop is necessary to get the max value across the histograms
# i.e. to get  -> current_incidence_max
# Initializing the original value with 0.
current_incidence_max = 0

for (current_country in countries)
{
  # Debugging step
  # current_country = countries[1]

  histogram_object_save =
    hist(
      x = incidence_data_standardized[,current_country],
      breaks = incidence_overall_bins,
      freq = FALSE,
      col = "darkolivegreen4",
      lwd = 0.5,
      # type = "p",
      pch = 19,
      main = paste(current_country),
      xlab = "Counts (Standardized by Population)",
      ylab = "Frequency",
      cex = 3,
      cex.axis = 1.55,
      cex.lab = 1.75,
      cex.main = 1.75,
      cex.sub = 1.75
    )

  # Finding max
  current_incidence_max = max(current_incidence_max, histogram_object_save$density)


  # End of -> for ( current_country in countries_to_select )
}


# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_incidence_histograms", graph_suffix, ".pdf", sep = ""), height = 26, width = 34)
# Defining the number of plots
par(par(mfrow = c(5, 8)), mar = c(5.1, 5.1, 5.1, 2.1))



for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]

  hist(
    x = incidence_data_standardized[,current_country],
    breaks = incidence_overall_bins,
    freq = FALSE,
    col = "darkolivegreen4",
    lwd = 0.5,
    # type = "p",
    pch = 19,
    main = paste(current_country),
    xlim = c( 0, max(incidence_overall_bins) * 1.65  ),
    # xlim = c(limits_golden[1], limits_golden[2]+3),
    # ylim = limits_golden,
    ylim = c(0, current_incidence_max),
    # xlab = "Time",
    xlab = "Counts (Standardized by Population)",
    ylab = "Frequency",
    # xaxt='n',
    # yaxt='n',
    cex = 3,
    cex.axis = 1.55,
    cex.lab = 1.75,
    cex.main = 1.75,
    cex.sub = 1.75
  )

  # Plotting veritcal line
  # Getting max values
  x_value_max = max(incidence_data_standardized[,current_country])

  lines(
    x = rep(x_value_max, 2),
    y = c(0, current_incidence_max),
    col = "darkorange3",
    lwd = 3,
    lty = 2
  )

  legend(
    x = "topright",
    inset = c(0.08, 0.04),
    legend = c(paste0("Density"), "Max"),
    col = "black",
    fill = c("darkolivegreen4", "darkorange3"),
    pt.cex = c(4, 2),
    # pch = c(19, 20),
    cex = 1.2,
    #ncol = 3
  )


  # End of -> for ( current_country in countries_to_select )
}


dev.off()








# mortality histograms

# This for loop is necessary to get the max value across the histograms
# i.e. to get  -> current_mortality_max
# Initializing the original value with 0.
current_mortality_max = 0



for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]

  histogram_object_save =
    hist(
      x = mortality_data_standardized[,current_country],
      breaks = mortality_overall_bins,
      freq = FALSE,
      col = "darkolivegreen4",
      lwd = 0.5,
      # type = "p",
      pch = 19,
      main = paste(current_country),
      xlab = "Counts (Standardized by Population)",
      ylab = "Frequency",
      cex = 3,
      cex.axis = 1.55,
      cex.lab = 1.75,
      cex.main = 1.75,
      cex.sub = 1.75
    )

  # Finding max
  current_mortality_max = max(current_mortality_max, histogram_object_save$density)


  # End of -> for ( current_country in countries_to_select )
}


# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_mortality_histograms", graph_suffix, ".pdf", sep = ""), height = 26, width = 34)
# Defining the number of plots
par(par(mfrow = c(5, 8)), mar = c(5.1, 5.1, 5.1, 2.1))



for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]

  hist(
    x = mortality_data_standardized[,current_country],
    breaks = mortality_overall_bins,
    freq = FALSE,
    col = "darkolivegreen4",
    lwd = 0.5,
    # type = "p",
    pch = 19,
    main = paste(current_country),
    xlim = c( 0, max(mortality_overall_bins) * 1.85  ),
    # xlim = c(limits_golden[1], limits_golden[2]+3),
    # ylim = limits_golden,
    ylim = c(0, current_mortality_max),
    # xlab = "Time",
    xlab = "Counts (Standardized by Population)",
    ylab = "Frequency",
    # xaxt='n',
    # yaxt='n',
    cex = 3,
    cex.axis = 1.55,
    cex.lab = 1.75,
    cex.main = 1.75,
    cex.sub = 1.75
  )

  # Plotting veritcal line
  # Getting max values
  x_value_max = max(mortality_data_standardized[,current_country])

  lines(
    x = rep(x_value_max, 2),
    y = c(0, current_mortality_max),
    col = "darkorange3",
    lwd = 3,
    lty = 2
  )


  if (current_country == "Russia") {
    legend(
      x = "topright",
      inset = c(0.08, 0.04),
      legend = c(paste0("Density"), "Max"),
      col = "black",
      fill = c("darkolivegreen4", "darkorange3"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 1.5
    )

    # End of -> if (current_country == "Russia")
  }


  if (current_country != "Russia") {
    legend(
      x = "topright",
      inset = c(0.08, 0.04),
      legend = c(paste0("Density"), "Max"),
      col = "black",
      fill = c("darkolivegreen4", "darkorange3"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 1.5
    )

    # End of -> if (current_country != "Russia")
  }



  # End of -> for ( current_country in countries_to_select )
}


dev.off()








# Defingin color palette
countries_to_select_reversre = rev(countries)
# Defining plotting colors
countries_colors_frame = data.frame(
  Country = countries_to_select_reversre,
  Color = rep("black", length(countries_to_select_reversre))
)
countries_colors_frame$Color = as.character(countries_colors_frame$Color)

# Overwriting colors
countries_colors_frame
for (current_country in countries_to_select_reversre)
{
  countries_colors_frame[countries_colors_frame$Country == current_country, 2] = randomcoloR::randomColor()
}
countries_colors_frame

# Same frame sorted
countries_colors_frame_alpahbetized = countries_colors_frame[with(countries_colors_frame, order(Country)), ]
countries_colors_frame_alpahbetized




# incidence


# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_incidence_plot", graph_suffix, ".pdf", sep = ""), height = 10, width = 16)
# Defining the number of plots
mar = c(5.6, 5.2, 5.1, 2.1)
# par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot (Belarus)
plot(
  x = incidence_data_standardized$Date,
  y = incidence_data_standardized[,countries_colors_frame$Country[1]],
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Incidence",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(incidence_overall_min, incidence_overall_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2

  # Plots themselves
  lines(
    x = incidence_data_standardized$Date,
    y = incidence_data_standardized[,countries_colors_frame$Country[current_country_index]],
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )


  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.04, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.25,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(incidence_data_standardized$Date))
final_date = as.integer(max(incidence_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(incidence_data_standardized$Date), by = round(length(incidence_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = incidence_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(incidence_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = incidence_overall_min
y_max_value = incidence_overall_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)

dev.off()











# mortality


# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_mortality_plot", graph_suffix, ".pdf", sep = ""), height = 10, width = 16)
# Defining the number of plots
mar = c(5.6, 5.2, 5.1, 2.1)
# par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot (Belarus)
plot(
  x = mortality_data_standardized$Date,
  y = mortality_data_standardized[,countries_colors_frame$Country[1]],
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Mortality",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(mortality_overall_min, mortality_overall_max * 3),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2

  
  # Plots themselves
  lines(
    x = mortality_data_standardized$Date,
    y = mortality_data_standardized[,countries_colors_frame$Country[current_country_index]],
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )


  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.04, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.25,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(mortality_data_standardized$Date))
final_date = as.integer(max(mortality_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(mortality_data_standardized$Date), by = round(length(mortality_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = mortality_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(mortality_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = mortality_overall_min
y_max_value = mortality_overall_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)

dev.off()





















# cumulative incidence


# This for loop is necessary to get the max value across the cumulative incidence
# i.e. to get  -> current_cumulative_incidence_max
# Initializing the original value with 0.
current_cumulative_incidence_max = 0

for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]

  # Finding max
  current_cumulative_incidence_max = max(current_cumulative_incidence_max, cumsum(incidence_data_standardized[,current_country]))


  # End of -> for ( current_country in countries_to_select )
}



# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_cumulative_incidence_plot", graph_suffix, ".pdf", sep = ""), height = 8, width = 16)
# Defining the number of plots
mar = c(5.6, 5.2, 5.1, 2.1)
# par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot (Belarus)
plot(
  x = incidence_data_standardized$Date,
  y = cumsum(incidence_data_standardized[,countries_colors_frame$Country[1]]),
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Cumulative Incidence",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(0, current_cumulative_incidence_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Cummulative Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2

  # Plots themselves
  lines(
    x = incidence_data_standardized$Date,
    y = cumsum(incidence_data_standardized[,countries_colors_frame$Country[current_country_index]]),
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )


  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.02, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.05,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(incidence_data_standardized$Date))
final_date = as.integer(max(incidence_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(incidence_data_standardized$Date), by = round(length(incidence_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = incidence_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(incidence_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = 0
y_max_value = current_cumulative_incidence_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)

dev.off()








# cumulative mortality


# This for loop is necessary to get the max value across the cumulative mortality
# i.e. to get  -> current_cumulative_mortality_max
# Initializing the original value with 0.
current_cumulative_mortality_max = 0

for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]

  # Finding max
  current_cumulative_mortality_max = max(current_cumulative_mortality_max, cumsum(mortality_data_standardized[,current_country]))


  # End of -> for ( current_country in countries_to_select )
}



# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_cumulative_mortality_plot", graph_suffix, ".pdf", sep = ""), height = 8, width = 16)
# Defining the number of plots
mar = c(5.6, 5.2, 5.1, 2.1)
# par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot (Belarus)
plot(
  x = mortality_data_standardized$Date,
  y = cumsum(mortality_data_standardized[,countries_colors_frame$Country[1]]),
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Cumulative Mortality",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(0, current_cumulative_mortality_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Cummulative Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2

  # Plots themselves
  lines(
    x = mortality_data_standardized$Date,
    y = cumsum(mortality_data_standardized[,countries_colors_frame$Country[current_country_index]]),
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )


  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.02, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.05,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(mortality_data_standardized$Date))
final_date = as.integer(max(mortality_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(mortality_data_standardized$Date), by = round(length(mortality_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = mortality_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(mortality_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = 0
y_max_value = current_cumulative_mortality_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)

dev.off()









# Fix 2023.02.11.
# Combined figure.


# Cumulative incidence and mortality combined

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



# cumulative incidence


# This for loop is necessary to get the max value across the cumulative incidence
# i.e. to get  -> current_cumulative_incidence_max
# Initializing the original value with 0.
current_cumulative_incidence_max = 0

for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]
  
  # Finding max
  current_cumulative_incidence_max = max(current_cumulative_incidence_max, cumsum(incidence_data_standardized[,current_country]))
  
  
  # End of -> for ( current_country in countries_to_select )
}


# cumulative mortality


# This for loop is necessary to get the max value across the cumulative mortality
# i.e. to get  -> current_cumulative_mortality_max
# Initializing the original value with 0.
current_cumulative_mortality_max = 0

for (current_country in countries)
{
  # Debugging step
  # current_country = countries_to_select[1]
  
  # Finding max
  current_cumulative_mortality_max = max(current_cumulative_mortality_max, cumsum(mortality_data_standardized[,current_country]))
  
  
  # End of -> for ( current_country in countries_to_select )
}






# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_cumulative_incidence_mortality_plot", graph_suffix, ".pdf", sep = ""), height = 16, width = 17)
# Defining the number of plots
# mar = c(5.6, 5.2, 5.1, 2.1)
par( par(mfrow=c(2,1)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# incidence

# First plot (Belarus)
plot(
  x = incidence_data_standardized$Date,
  y = cumsum(incidence_data_standardized[,countries_colors_frame$Country[1]]),
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Cumulative Incidence",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(0, current_cumulative_incidence_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Cummulative Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2
  
  # Plots themselves
  lines(
    x = incidence_data_standardized$Date,
    y = cumsum(incidence_data_standardized[,countries_colors_frame$Country[current_country_index]]),
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )
  
  
  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.02, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.05,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(incidence_data_standardized$Date))
final_date = as.integer(max(incidence_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(incidence_data_standardized$Date), by = round(length(incidence_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = incidence_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(incidence_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = 0
y_max_value = current_cumulative_incidence_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)


# Label A
label_function(label_value = "A", label_cex = 5)



# mortality

# First plot (Belarus)
plot(
  x = mortality_data_standardized$Date,
  y = cumsum(mortality_data_standardized[,countries_colors_frame$Country[1]]),
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Cumulative Mortality",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(0, current_cumulative_mortality_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Cummulative Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2
  
  # Plots themselves
  lines(
    x = mortality_data_standardized$Date,
    y = cumsum(mortality_data_standardized[,countries_colors_frame$Country[current_country_index]]),
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )
  
  
  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.02, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.05,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(mortality_data_standardized$Date))
final_date = as.integer(max(mortality_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(mortality_data_standardized$Date), by = round(length(mortality_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = mortality_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(mortality_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = 0
y_max_value = current_cumulative_mortality_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)


# Label B
label_function(label_value = "B", label_cex = 5)



dev.off()



















# Incidence and mortality combined



# Generating pdf output.
pdf(paste("../Plots/part01_02_raw_data_figure_production_incidence_mortality_plot", graph_suffix, ".pdf", sep = ""), height = 18, width = 17)
# Defining the number of plots
# mar = c(5.6, 5.2, 5.1, 2.1)
par( par(mfrow=c(2,1)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# incidence


# First plot (Belarus)
plot(
  x = incidence_data_standardized$Date,
  y = incidence_data_standardized[,countries_colors_frame$Country[1]],
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Incidence",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(incidence_overall_min, incidence_overall_max),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2
  
  # Plots themselves
  lines(
    x = incidence_data_standardized$Date,
    y = incidence_data_standardized[,countries_colors_frame$Country[current_country_index]],
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )
  
  
  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.04, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.25,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(incidence_data_standardized$Date))
final_date = as.integer(max(incidence_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(incidence_data_standardized$Date), by = round(length(incidence_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = incidence_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(incidence_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = incidence_overall_min
y_max_value = incidence_overall_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)


# Label A
label_function(label_value = "A", label_cex = 5)





# mortality

# First plot (Belarus)
plot(
  x = mortality_data_standardized$Date,
  y = mortality_data_standardized[,countries_colors_frame$Country[1]],
  col = "darkturquoise",
  # col = color_01,
  lwd = 3,
  # pch = 16,
  # pch = shape_01,
  # pch = 17,
  type = "l",
  # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
  main = "COVID-19 Standardized Mortality",
  # xlim = c( combined_date_min,  combined_date_max  ),
  ylim = c(mortality_overall_min, mortality_overall_max * 3),
  # ylim = c(0, y_max_value_current * 1.2  ),
  # xlab = "Time",
  xlab = "",
  ylab = "Counts (Standardized by Population)",
  xaxt = "n",
  yaxt = "n",
  cex = 2,
  cex.axis = 1.55,
  cex.lab = 1.55,
  cex.main = 1.55,
  cex.sub = 2
)


# Note -> In this loop Ukraine will be plotted twice since it was plotted a above.

for (current_country_index in c(1:dim(countries_colors_frame)[1]))
{
  # Debugging step
  # current_country_index = 2
  
  
  # Plots themselves
  lines(
    x = mortality_data_standardized$Date,
    y = mortality_data_standardized[,countries_colors_frame$Country[current_country_index]],
    # col = "darkblue",
    col = countries_colors_frame$Color[current_country_index],
    # col = color_01,
    lwd = 3,
    pch = 19,
    # pch = shape_01,
    # pch = 17,
    type = "l"
  )
  
  
  # End of -> for ( current_country in countries_to_select )
}

legend(
  x = "topleft",
  inset = c(0.04, 0.04),
  legend = countries_colors_frame_alpahbetized$Country,
  col = "black",
  fill = countries_colors_frame_alpahbetized$Color,
  pt.cex = c(4, 2),
  # pch = c(19, 20),
  cex = 1.25,
  ncol = 3
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date = as.integer(min(mortality_data_standardized$Date))
final_date = as.integer(max(mortality_data_standardized$Date))
number_of_dates = final_date - initial_date


# Indexes to display
x_indexes_to_display = seq(from = 1, to = length(mortality_data_standardized$Date), by = round(length(mortality_data_standardized$Date)/63) )
# x_indexes_to_display[1] = 1
# Actual lab elements
x_tlab = mortality_data_standardized$Date[x_indexes_to_display]
# ctual lab labels
x_lablist = substr(x = as.character(mortality_data_standardized$Date[x_indexes_to_display]), start = 1, stop = 13)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value = mortality_overall_min
y_max_value = mortality_overall_max
y_tlab = seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist = as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)


# Label B
label_function(label_value = "B", label_cex = 5)



dev.off()































