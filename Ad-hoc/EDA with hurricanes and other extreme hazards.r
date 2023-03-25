# Fitting frequency distributions for data

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])


# How would you fit on these distributions?




# EDA on extreme weather events - checking events that are not independent and wide sweeping

extreme_hazards <- hd[InflatedPropertyDamage > 30000000]

for (i in unique(extreme_hazards[, HazardEventGrouped])) {
  
  plot <- ggplot() + geom_line(data = extreme_hazards[HazardEventGrouped == i], 
                               aes(x = Year, y = as.numeric(PropertyDamage), col = factor(Region)), 
                               size = 0.6) + ggtitle(i)
  
  print(plot)
  
}



# Fitting aggregate distribution on hazards

# Maybe model these Property Damages separately

hazard_data__outliers <- hd[PropertyDamage == 3086384400 |
                            PropertyDamage == 7877651400 |
                            PropertyDamage == 1132181544 | 
                            PropertyDamage == 1120498620]

# What to do with these hurricanes?


