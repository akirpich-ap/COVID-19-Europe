# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2023.02.11. ask
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
summaries_by_country_incidence_mortality_pr = data.frame(Country = countries, Incidence      = rep(0, length(countries)), 
                                                                              Mortality      = rep(0, length(countries)), 
                                                                              Incidence_prop = rep(0, length(countries)),
                                                                              Mortality_prop = rep(0, length(countries)))

# Filing the proportions
for (current_country in countries)
{
  # Debugging
  # current_country <- countries[1]
  

  # Extracting time series 
  # incidence 
  text_incidence <- paste0( "country_data_incidence <- incidence_data_selected_fixed$`", current_country, "`" ) 
  eval(parse(text = text_incidence))    
  # Mortality
  text_mortality <- paste0( "country_data_mortality <- mortality_data_selected_fixed$`", current_country, "`" ) 
  eval(parse(text = text_mortality))    
  
  # Saving the results
  # Extracting the row
  which_row_current_country <- which(summaries_by_country_incidence_mortality_pr$Country == current_country)
  # Filling the rows
  # incidence
  summaries_by_country_incidence_mortality_pr$Incidence[which_row_current_country] <- cumsum(country_data_incidence)[length(country_data_incidence)]
  # mortality
  summaries_by_country_incidence_mortality_pr$Mortality[which_row_current_country] <- cumsum(country_data_mortality)[length(country_data_mortality)]
  
  # incidence proportion
  summaries_by_country_incidence_mortality_pr$Incidence_prop[which_row_current_country] <- 
     cumsum(country_data_incidence)[length(country_data_incidence)]/(cumsum(country_data_incidence)[length(country_data_incidence)] + cumsum(country_data_mortality)[length(country_data_mortality)])
  # mortality proportion
  summaries_by_country_incidence_mortality_pr$Mortality_prop[which_row_current_country] <- 
    cumsum(country_data_mortality)[length(country_data_mortality)]/(cumsum(country_data_incidence)[length(country_data_incidence)] + cumsum(country_data_mortality)[length(country_data_mortality)])
  
  # extra check
  (summaries_by_country_incidence_mortality_pr$Incidence_prop[which_row_current_country] + summaries_by_country_incidence_mortality_pr$Mortality_prop[which_row_current_country]) == 1

# End of -> for (current_country in countries)  
}


# Creating sorted by mortality proportion version
summaries_by_country_incidence_mortality_pr_sorted <- summaries_by_country_incidence_mortality_pr[order(summaries_by_country_incidence_mortality_pr$Mortality_prop),]


# RData export
# original
summaries_by_country_incidence_mortality_pr_rdata_path = paste0("../R_Data/summaries_by_country_incidence_mortality_pr.RData")
save(summaries_by_country_incidence_mortality_pr, file = summaries_by_country_incidence_mortality_pr_rdata_path)

# sorted
summaries_by_country_incidence_mortality_pr_sorted_rdata_path = paste0("../R_Data/summaries_by_country_incidence_mortality_pr_sorted.RData")
save(summaries_by_country_incidence_mortality_pr_sorted, file = summaries_by_country_incidence_mortality_pr_sorted_rdata_path)



# CSV table export
# original
summaries_by_country_incidence_mortality_pr_csv_path = paste0("../R_Output/summaries_by_country_incidence_mortality_pr.csv")
write.table(summaries_by_country_incidence_mortality_pr, file = summaries_by_country_incidence_mortality_pr_csv_path, sep = ",", quote = TRUE, row.names = FALSE)

# sorted
summaries_by_country_incidence_mortality_pr_sorted_csv_path = paste0("../R_Output/summaries_by_country_incidence_mortality_pr_sorted.csv")
write.table(summaries_by_country_incidence_mortality_pr_sorted, file = summaries_by_country_incidence_mortality_pr_sorted_csv_path, sep = ",", quote = TRUE, row.names = FALSE)




# Computing proportions
# original
proportions_for_barplot <- t(summaries_by_country_incidence_mortality_pr[, c("Mortality_prop", "Incidence_prop")])
colnames(proportions_for_barplot) <- summaries_by_country_incidence_mortality_pr$Country
# Duplicate with percentage range up to 90%.
proportions_for_barplot_fixed     <- proportions_for_barplot[1,]
# proportions_for_barplot_fixed[2,] <- proportions_for_barplot[2,] - 0.958

# sorted
proportions_for_barplot_sorted <- t(summaries_by_country_incidence_mortality_pr_sorted[, c("Mortality_prop", "Incidence_prop")])
colnames(proportions_for_barplot_sorted) <- summaries_by_country_incidence_mortality_pr_sorted$Country
# Duplicate with percentage range up to 90%.
proportions_for_barplot_fixed_sorted     <- proportions_for_barplot_sorted[1,]
# proportions_for_barplot_fixed_sorted[2,] <- proportions_for_barplot_sorted[2,] - 0.958




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
pdf(paste("../Plots/part03_01_raw_data_cummultative_ratios_dead_to_infected", graph_suffix, ".pdf", sep = ""), height = 14, width = 16)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 1)), mar = c(16.5, 5.1, 5.1, 2.1))


barplot( proportions_for_barplot_fixed, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of Death Among COVID-19 Cases",
         # names.arg = colnames(proportions_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(proportions_for_barplot_fixed)[2]),
         # xlim = c(1, dim(proportions_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topright", bg="white",
        inset = c(0.08, 0.04),
        legend = c("Mortality Proportion"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 2
)

# label_function(label_text = "")

dev.off()













# Generating pdf output.
pdf(paste("../Plots/part03_01_raw_data_cummultative_ratios_dead_to_infected_sorted", graph_suffix, ".pdf", sep = ""), height = 14, width = 16)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 1)), mar = c(16.5, 5.1, 5.1, 2.1))


barplot( proportions_for_barplot_fixed_sorted, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of Death Among COVID-19 Cases",
         # names.arg = colnames(proportions_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(proportions_for_barplot_fixed)[2]),
         # xlim = c(1, dim(proportions_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.08, 0.04),
        legend = c("Mortality Proportion"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 2
)

# label_function(label_text = "")

dev.off()










# Fix 2023.02.15. ask
# Combinign the two figures.


# Generating pdf output.
pdf(paste("../Plots/part03_01_raw_data_cummultative_ratios_dead_to_infected_both", graph_suffix, ".pdf", sep = ""), height = 16, width = 32)
# Defining the number of plots
# par(par(mfrow = c(2, 1)), mar = c(5.1, 5.1, 5.1, 2.1))
par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 5.1, 2.1))


# Plot A

barplot( proportions_for_barplot_fixed, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of Death Among COVID-19 Cases",
         # names.arg = colnames(proportions_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(proportions_for_barplot_fixed)[2]),
         # xlim = c(1, dim(proportions_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topright", bg="white",
        inset = c(0.08, 0.04),
        legend = c("Mortality Proportion"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 2
)

# Label A
label_function(label_value = "A", label_cex = 6)



# Plot B

barplot( proportions_for_barplot_fixed_sorted, col=c("darkturquoise"), 
         # legend = TRUE, 
         border =  TRUE, 
         # xlim = c(1, 5), 
         # args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of Death Among COVID-19 Cases",
         # names.arg = colnames(proportions_for_barplot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2,
         # xlim = c(1, 1.6*dim(proportions_for_barplot_fixed)[2]),
         # xlim = c(1, dim(proportions_for_barplot_fixed)[2]),
         cex = 2,
         las = 2
)

legend( x = "topleft", bg="white",
        inset = c(0.08, 0.04),
        legend = c("Mortality Proportion"),
        col = "black",
        fill = c("darkturquoise"),
        pt.cex = c(1, 1),
        # pch = c(19, 20),
        cex = 2
)

label_function(label_value = "B", label_cex = 6)

dev.off()


















