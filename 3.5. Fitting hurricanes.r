# Fitting frequency distributions for data

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])


# Fitting aggregate distribution on hazards

# Maybe model these Property Damages separately

hazard_data__outliers <- hd[PropertyDamage == 3086384400 |
                            PropertyDamage == 7877651400 |
                            PropertyDamage == 1132181544 | 
                            PropertyDamage == 1120498620]


