library(ggplot2)
library(fitdistrplus)
library(data.table)
library(Rfast)


hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

#### Fitting frequency distribution to lower data ####

low_data <- hd[PropertyDamage > 0 & InflatedPropertyDamage < 1000000]
low_data_freq <- low_data[, .N, .(Region, Year)]
region_list <- unique(hd[, Region])

# Creating frequency tables

region_freq <- data.table(Region = 0, Year = 0, N = 0)

for (i in region_list) {

  region_base <- data.table(Region = rep(i, 61), Year = seq(1960, 2020))
  
  region_specific <- merge(region_base, low_data_freq, by = c("Region", "Year"), all.x = T)
  
  region_freq <- rbind(region_freq, region_specific)

}

region_freq[is.na(N), N := 0]

region_freq <- region_freq[-1]

# Performing dispersion tests

dispersion_test <- data.table(Region = "test", pvalue = 0)

for (i in region_list) {
  
  hazard_data_lower <- region_freq[Region == i, N]
  
  dispersion_test <- rbind(dispersion_test, data.table(Region = i,
                                                       pvalue = poisdisp.test(hazard_data_lower, alternative = "over", logged = FALSE)[2]))
  
}

#View(dispersion_test)

# Fitting negative binomial models to count data

#pdf("C:/Users/PC/Documents/Uni/4001/Analysis/Nbinom_region_lower.pdf")

nb_params_lower <- data.table(Region = "test", Size = 0, Mu = 0)

for (i in region_list) {
  
  titleplot <- ggplot() + ggtitle(paste0("Region ",i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(titleplot)
  
  hazard_data_lower <- region_freq[Region == i, N]
  
  hazard_nb <- fitdist(hazard_data_lower, "nbinom", method = "mle")
  
  plot(hazard_nb)
  
  nb_params_lower <- rbind(nb_params_lower, data.table(Region = i, Size = hazard_nb$estimate[1], Mu = hazard_nb$estimate[2]))

}

nb_params_lower <- nb_params_lower[-1]

#dev.off()


#### Fitting frequency to middle data ####

mid_data <- hd[PropertyDamage > 1000000 & InflatedPropertyDamage < 30000000]
mid_data_freq <- mid_data[, .N, .(Region, Year)]

# Creating frequency tables

region_freq <- data.table(Region = 0, Year = 0, N = 0)

for (i in region_list) {
  
  region_base <- data.table(Region = rep(i, 61), Year = seq(1960, 2020))
  
  region_specific <- merge(region_base, mid_data_freq, by = c("Region", "Year"), all.x = T)
  
  region_freq <- rbind(region_freq, region_specific)
  
}

region_freq[is.na(N), N := 0]

region_freq <- region_freq[-1]

# Performing dispersion tests

dispersion_test <- data.table(Region = "test", pvalue = 0)

for (i in region_list) {
  
  hazard_data_mid <- region_freq[Region == i, N]
  
  dispersion_test <- rbind(dispersion_test, data.table(Region = i,
                                                       pvalue = poisdisp.test(hazard_data_mid, alternative = "over", logged = FALSE)[2]))
  
}

#View(dispersion_test)

# Fitting negative binomial models to count data

#pdf("C:/Users/PC/Documents/Uni/4001/Analysis/Nbinom_region_mid.pdf")

nb_params_mid <- data.table(Region = "test", Size = 0, Mu = 0)

for (i in region_list) {
  
  titleplot <- ggplot() + ggtitle(paste0("Region ",i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(titleplot)
  
  hazard_data_mid <- region_freq[Region == i, N]
  
  hazard_nb <- fitdist(hazard_data_mid, "nbinom", method = "mle")
  
  plot(hazard_nb)
  
  nb_params_mid <- rbind(nb_params_mid, data.table(Region = i, Size = hazard_nb$estimate[1], Mu = hazard_nb$estimate[2]))
  
}

nb_params_mid <- nb_params_mid[-1]

#dev.off()

#### Fitting frequency to upper data ####

upper_data <- hd[!(Key %in% c("H2828", "H2827", "W3232", 
                            "D62", "D63", "D64", "D65", "D66", "D67",
                            "F2702", "F2737", "F2738", "F2741", "F2746", "F2756",
                            "H810", "H811", "H812", "H813", "H814")) &
                 InflatedPropertyDamage > 30000000]

upper_data <- upper_data[, .N, .(Region, Year)]

# Creating frequency tables

region_freq <- data.table(Region = 0, Year = 0, N = 0)

for (i in region_list) {
  
  region_base <- data.table(Region = rep(i, 61), Year = seq(1960, 2020))
  
  region_specific <- merge(region_base, upper_data, by = c("Region", "Year"), all.x = T)
  
  region_freq <- rbind(region_freq, region_specific)
  
}

region_freq[is.na(N), N := 0]

region_freq <- region_freq[-1]

# Performing dispersion tests

dispersion_test <- data.table(Region = "test", pvalue = 0)

for (i in region_list) {
  
  hazard_data_upper <- region_freq[Region == i, N]
  
  dispersion_test <- rbind(dispersion_test, data.table(Region = i,
                                                       pvalue = poisdisp.test(hazard_data_upper, alternative = "under", logged = FALSE)[2]))
  
}

#View(dispersion_test)

# Fitting poisson models to count data

#pdf("C:/Users/PC/Documents/Uni/4001/Analysis/Nbinom_region_upper.pdf")

pois_params_upper <- data.table(Region = "test", Lambda = 0)

for (i in region_list) {
  
  titleplot <- ggplot() + ggtitle(paste0("Region ",i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(titleplot)
  
  hazard_data_upper<- region_freq[Region == i, N]
  
  if (sum(hazard_data_upper) > 0) {
  
    hazard_pois <- fitdist(hazard_data_upper, "pois", method = "mle")
    
    plot(hazard_pois)
    
    pois_params_upper <- rbind(pois_params_upper, data.table(Region = i, Lambda = hazard_pois$estimate[1]))
    
  }
  
}

pois_params_upper <- pois_params_upper[-1]

#dev.off()

#### Fitting frequency to extreme data ####

freak_data <- freak_hazards <- hd[Key %in% c("H2828", "H2827", "W3232", 
                                             "D62", "D63", "D64", "D65", "D66", "D67",
                                             "F2702", "F2737","F2738", "F2741", "F2746", "F2756",
                                             "H810", "H811", "H812", "H813", "H814")]

  # Aggregating so each freak accident is treated as one event

freak_data[, Freak_Key := paste0(substr(Key, 1, 1), Year)]

freak_hazard_grouped <- freak_data[, .(N = 1), .(Freak_Key, Year)]

freak_hazard_grouped <- freak_hazard_grouped[, .(N, Year)]

# Creating frequency tables

region_freq <- data.table(Year = 0, N = 0)

region_base <- data.table(Year = seq(1960, 2020))
  
region_specific <- merge(region_base, freak_hazard_grouped, by = c("Year"), all.x = T)
  
region_freq <- rbind(region_freq, region_specific)
  
region_freq[is.na(N), N := 0]

region_freq <- region_freq[-1]

# Performing dispersion tests

dispersion_test <- data.table(pvalue = 0)

hazard_data_freak <- region_freq[, N]
  
dispersion_test <- rbind(dispersion_test, data.table(pvalue = poisdisp.test(hazard_data_freak, alternative = "under", logged = FALSE)[2]))
  
#View(dispersion_test)

# Fitting poisson model to count data

#pdf("C:/Users/PC/Documents/Uni/4001/Analysis/Nbinom_region_freak.pdf")

pois_params_freak <- data.table(Region = 0, Lambda = 0)

titleplot <- ggplot() + ggtitle(paste0("Freak")) + 
  theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
print(titleplot)

hazard_data_freak<- region_freq[, N]
  
if (sum(hazard_data_freak) > 0) {
  
  hazard_pois <- fitdist(hazard_data_freak, "pois", method = "mle")
  
  plot(hazard_pois)
  
  pois_params_freak <- rbind(pois_params_freak, data.table(Region = 0, Lambda = hazard_pois$estimate[1]))

}

pois_params_freak <- pois_params_freak[-1]

#dev.off()
















