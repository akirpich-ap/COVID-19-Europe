# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.06.02. ask
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
# library(readxl)

# Loading library(rjson) for json files.
# library(rjson)

# Library for read_csv
# library(readr)

# To convert dates from text to dates
# library(lubridate)


# This package is used for dynamic time wrapping (dtw).
# library(dtw)



# Reading data
# Incidence from RData file.
load(file = paste("../R_Data/incidence_data_selected_fixed.RData"))
# Mortality as RData file.
load(file = paste("../R_Data/mortality_data_selected_fixed.RData"))
load(file = paste("../R_Data/countries.RData"))
graph_suffix = "_eu"


# Loading standardized versions
# incidence
load( file = paste("../R_Data/incidence_data_standardized.RData"))
# mortality
load( file = paste("../R_Data/mortality_data_standardized.RData"))
ls()







# Exploratory summaries
summaries_by_country_incidence_mortality_cc = data.frame(Country = countries, cc_statistic   = rep(0, length(countries)))

# Filing Ratios Kolmogorov-Smirnov
for (current_country in countries)
{
  # Debugging
  # current_country <- countries[1]
  

  # Extracting time series 
  # incidence 
  text_incidence <- paste0( "country_data_incidence <- incidence_data_selected_fixed_plus_standardized$`", current_country, "`" ) 
  eval(parse(text = text_incidence))    
  # Mortality
  text_mortality <- paste0( "country_data_mortality <- mortality_data_selected_fixed_plus_standardized$`", current_country, "`" ) 
  eval(parse(text = text_mortality))    
  
  
  # Pointwise Absolute Differences
  
  # Standardizing
  # incidence
  country_data_incidence_standardized <- country_data_incidence/max(country_data_incidence, country_data_mortality) * 1000 
  # mortality
  country_data_mortality_standardized <- country_data_mortality/max(country_data_incidence, country_data_mortality) * 1000 
  
      
  # Saving the DTW results  
  ts_test_current_result <- 1 - max( ccf(x = country_data_incidence_standardized, y = country_data_mortality_standardized)$acf )
  
  # Extracting the row
  which_row_current_country <- which(summaries_by_country_incidence_mortality_cc$Country == current_country)
  
  # Saving the results into the frame
  summaries_by_country_incidence_mortality_cc$cc_statistic[which_row_current_country] <- ts_test_current_result


# End of -> for (current_country in countries)  
}


# Creating sorted by mortality proportion version
summaries_by_country_incidence_mortality_cc_sorted <- summaries_by_country_incidence_mortality_cc[order(summaries_by_country_incidence_mortality_cc$cc_statistic),]


# RData export
# original
summaries_by_country_incidence_mortality_cc_rdata_path = paste0("../R_Data/summaries_by_country_incidence_mortality_cc.RData")
save(summaries_by_country_incidence_mortality_cc, file = summaries_by_country_incidence_mortality_cc_rdata_path)

# sorted
summaries_by_country_incidence_mortality_cc_sorted_rdata_path = paste0("../R_Data/summaries_by_country_incidence_mortality_cc_sorted.RData")
save(summaries_by_country_incidence_mortality_cc_sorted, file = summaries_by_country_incidence_mortality_cc_sorted_rdata_path)



# CSV table export
# original
summaries_by_country_incidence_mortality_cc_csv_path = paste0("../R_Output/summaries_by_country_incidence_mortality_cc.csv")
write.table(summaries_by_country_incidence_mortality_cc, file = summaries_by_country_incidence_mortality_cc_csv_path, sep = ",", quote = TRUE, row.names = FALSE)

# sorted
summaries_by_country_incidence_mortality_cc_sorted_csv_path = paste0("../R_Output/summaries_by_country_incidence_mortality_cc_sorted.csv")
write.table(summaries_by_country_incidence_mortality_cc_sorted, file = summaries_by_country_incidence_mortality_cc_sorted_csv_path, sep = ",", quote = TRUE, row.names = FALSE)



# Fix 2022.02.15.

# Computing standardized proportions i.e. divided by max and multipld by 1000
# original
counts_for_barplot <- 1000*t(summaries_by_country_incidence_mortality_cc[, c("cc_statistic")])/max(summaries_by_country_incidence_mortality_cc[, c("cc_statistic")])
colnames(counts_for_barplot) <- summaries_by_country_incidence_mortality_cc$Country

# sorted
counts_for_barplot_sorted <- 1000*t(summaries_by_country_incidence_mortality_cc_sorted[, c("cc_statistic")])/max((summaries_by_country_incidence_mortality_cc_sorted[, c("cc_statistic")]))
colnames(counts_for_barplot_sorted) <- summaries_by_country_incidence_mortality_cc_sorted$Country







# Creating function for labels.
label_function <- function(label_text = "")
{
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_text
  x <- x[1] + strwidth(txt, cex=4) * 6 / 5
  y <- y[2] - strheight(txt, cex=4) * 6 / 5
  text(x, y, txt, cex = 4)
  
  # End of -> label_function <- function(label_text = "X")  
}  









# Generating pdf output.
pdf(paste("../Plots/part04_06_cross_correlation_distance_ratios_dead_to_infected", graph_suffix, ".pdf", sep = ""), height = 14, width = 16)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 1)), mar = c(16.5, 5.1, 5.1, 2.1))


barplot( counts_for_barplot, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Cross-Correlation Distance for Comparisons of COVID-19 Mortality vs Incidence",
         # names.arg = colnames(counts_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(counts_for_barplot_fixed)[2]),
         # xlim = c(1, dim(counts_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.18, 0.00),
        legend = c("Cross-Correlation Distance"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 1.75
)

label_function(label_text = "")

dev.off()













# Generating pdf output.
pdf(paste("../Plots/part04_06_cross_correlation_distance_ratios_dead_to_infected_sorted", graph_suffix, ".pdf", sep = ""), height = 14, width = 16)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 1)), mar = c(16.5, 5.1, 5.1, 2.1))


barplot( counts_for_barplot_sorted, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Cross-Correlation Distance for Comparisons of COVID-19 Mortality vs Incidence",
         # names.arg = colnames(counts_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(counts_for_barplot_fixed)[2]),
         # xlim = c(1, dim(counts_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.18, 0.00),
        legend = c("Cross-Correlation Distance"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 1.75
)

label_function(label_text = "")

dev.off()





# Fix 2023.02.15. ask
# Combined two figures in one.

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
  
  
  # End of -> label_function <- function(label_value = "A", label_cex = 4) {
}  







# Generating pdf output.
pdf(paste("../Plots/part04_06_cross_correlation_distance_ratios_dead_to_infected_both", graph_suffix, ".pdf", sep = ""), height = 16, width = 32)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))

# Plot A

barplot( counts_for_barplot, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Cross-Correlation Distance for Comparisons of COVID-19 Mortality vs Incidence",
         # names.arg = colnames(counts_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(counts_for_barplot_fixed)[2]),
         # xlim = c(1, dim(counts_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.18, 0.00),
        legend = c("Cross-Correlation Distance"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 1.75
)

# Label A
label_function(label_value = "A", label_cex = 6)



# Plot B

barplot( counts_for_barplot_sorted, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Cross-Correlation Distance for Comparisons of COVID-19 Mortality vs Incidence",
         # names.arg = colnames(counts_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(counts_for_barplot_fixed)[2]),
         # xlim = c(1, dim(counts_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.18, 0.00),
        legend = c("Cross-Correlation Distance"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 1.75
)


# Label B
label_function(label_value = "B", label_cex = 6)


dev.off()






















