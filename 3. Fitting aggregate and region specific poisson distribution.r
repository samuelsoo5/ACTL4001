library(ggplot2)
library(fitdistrplus)
library(data.table)
library(Rfast)

# Fitting count distributions on data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

hazard_data <- hd[PropertyDamage > 0 & InflatedPropertyDamage < 30000000]
hazard_data_freq <- hazard_data[, .N, .(Region, Year)]
region_list <- unique(hd[, Region])

# Creating frequency tables

region_freq <- data.table(Region = 0, Year = 0, N = 0)

for (i in region_list) {

region_base <- data.table(Region = rep(i, 61), Year = seq(1960, 2020))

region_specific <- merge(region_base, hazard_data_freq, by = c("Region", "Year"), all.x = T)

region_freq <- rbind(region_freq, region_specific)

}

region_freq[is.na(N), N := 0]

# Performing dispersion tests

dispersion_test <- data.table(Region = "test", pvalue = 0)

for (i in region_list) {
  
  hazard_data_lower <- region_freq[Region == i, N]
  
  dispersion_test <- rbind(dispersion_test, data.table(Region = i,
                                                       pvalue = poisdisp.test(hazard_data_lower, alternative = "over", logged = FALSE)[2]))
  
}

View(dispersion_test)

# Based on results of dispersion test, all data is overdispersed

# Fitting negative binomial models to count data

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/Nbinom_region.pdf")

nb_params <- data.table(Region = "test", Size = 0, Prob = 0)

for (i in region_list) {
  
  titleplot <- ggplot() + ggtitle(paste0("Region ",i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(titleplot)
  
  hazard_data_lower <- hazard_data_freq[Region == i, N]
  
  hazard_nb <- fitdist(hazard_data_lower, "nbinom", method = "mle")
  
  plot(hazard_nb)
  
  nb_params <- rbind(nb_params, data.table(Region = i, Size = hazard_nb$estimate[1], Prob = hazard_nb$estimate[2]))

}

# Fitting a frequency model to aggregate natural disasters (high damage)

titleplot <- ggplot() + ggtitle(paste0("Aggregate")) + 
  theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
print(titleplot)

hazard_data_upper<- hd[PropertyDamage != 3086384400 & 
                       PropertyDamage != 7877651400 &
                       PropertyDamage != 1132181544 &
                       PropertyDamage != 1120498620 &
                       InflatedPropertyDamage > 30000000]

hazard_data_upper_freq <- hazard_data_upper[, .N, Year]

# Creating frequency table for total

region_base <- data.table(Year = seq(1960, 2020))
  
total_freq <- merge(region_base, hazard_data_upper_freq, by = c("Year"), all.x = T)

total_freq[is.na(N), N := 0]

# Performing dispersion test

print(poisdisp.test(total_freq[, N], alternative = "over", logged = FALSE)[2])

# Fitting negative binomial model

hazard_nb <- fitdist(total_freq[, N], "nbinom", method = "mle")

plot(hazard_nb)

nb_params <- rbind(nb_params, data.table(Region = i, Size = hazard_nb$estimate[1], Prob = hazard_nb$estimate[2]))

dev.off()













