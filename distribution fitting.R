# Loading packages
library(data.table)
library(ggplot2)
library(fitdistrplus)
library(actuar)


# Loading in data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")


# Plotting time series boxplots

for (i in unique(hd[, HazardEventGrouped])) {
  plot <- ggplot() + geom_boxplot(data = hd[HazardEventGrouped == i], 
                          aes(x = factor(Year), y = as.integer(PropertyDamage), group = factor(Year))) + 
    ggtitle(i)
  
  print(plot)
}

# Plotting histogram of distribution
for (i in unique(hd[, HazardEventGrouped])) {
  plot <- ggplot() + geom_histogram(data = hd[HazardEventGrouped == i],
                            aes(x = as.integer(PropertyDamage)), bins = 50) +
      ggtitle(i)
  
  print(plot)
}

# Plotting distribution of total damages per year for each accident

for (i in unique(hd[, HazardEventGrouped])) {
  
  plot <- ggplot() + geom_bar(stat = "identity", data = hd[HazardEventGrouped == i], 
                    aes(x = factor(Year), y = as.integer(PropertyDamage))) + ggtitle(i)
  
  print(plot)
  
}

# Checking independence of hazards by region

for (i in unique(hd[, HazardEventGrouped])) {
  
  plot <- ggplot() + geom_line(data = hd[HazardEventGrouped == i], 
                      aes(x = Year, y = as.integer(PropertyDamage), col = factor(Region)), 
                      size = 0.6) + ggtitle(i)
  
}
  




# Miscellaneous plots

  # Focusing solely on Severe Storm/ThunderStorm/ Wind
  
  cdf <- ecdf(hd[HazardEventGrouped == "Severe Storm/ThunderStorm/Wind", PropertyDamage])
  
  plot(cdf)






