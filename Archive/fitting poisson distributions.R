library(extRemes)
library(evir)
library(evmix) # gpd density functions
library(goft) #gp_fit
library(POT) #fit gpd
library(tea) #
library(ggplot2)
library(fitdistrplus)
library(data.table)

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

hazard_list <- unique(hd[, HazardEventGrouped])

# Run from here

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/poisson_fits.pdf")

parameter_estimates <- data.table(Hazard = "test", Region = "test", Upper_Lower = "test", Lambda = 0)

for (i in hazard_list) {
  
  print(i)
  
  hazard_data <- hd[HazardEventGrouped == i, .N, .(Region, Year)]
  
  for (j in unique(hd[, Region])) {
    
    hazard_data_lower <- hazard_data[Year < 2010 & Region == j]
    hazard_data_upper <- hazard_data[Year >= 2010 & Region == j]
    
    if (nrow(hazard_data_upper) > 1){
      hazard_pois_upper <- fitdist(as.numeric(hazard_data_upper[, N]), "pois", method = "mle")
      titleplot <- ggplot() + ggtitle(paste0(i, "\n Region ", j," (Upper)\n", "lambda =", toString(round(hazard_pois_upper$estimate, 2)))) + 
        theme_void() + theme(plot.title = element_text(size = 30, vjust = -25, hjust = 0.4))
      print(titleplot)
      plot(hazard_freq_pois)
      parameter_estimates <- rbind(parameter_estimates, data.table(Hazard = i, Region = j, Upper_Lower = "Upper", Lambda = hazard_pois_upper$estimate))
    }
    
    if (nrow(hazard_data_lower) > 1){
      hazard_pois_lower <- fitdist(as.numeric(hazard_data_lower[, N]), "pois", method = "mle")
      titleplot <- ggplot() + ggtitle(paste0(i, "\n Region ", j," (Lower)\n", "lambda =", toString(round(hazard_pois_lower$estimate, 2)))) + 
      theme_void() + theme(plot.title = element_text(size = 30, vjust = -25, hjust = 0.4))
      print(titleplot)
      plot(hazard_freq_pois)
      parameter_estimates <- rbind(parameter_estimates, data.table(Hazard = i, Region = j, Upper_Lower = "Lower",Lambda = hazard_pois_lower$estimate))
    }
    
  }

}

dev.off()

View(parameter_estimates)


pdf("C:/Users/PC/Documents/Uni/4001/Analysis/poisson_fits_aggregate.pdf")

for (i in hazard_list) {
  
  print(i)
  
  hazard_data <- hd[HazardEventGrouped == i, .N, .(Region, Year)]
    
  hazard_data_lower <- hazard_data[Year < 2010]
  hazard_data_upper <- hazard_data[Year >= 2010]
  
  if (nrow(hazard_data_upper) > 1){
    hazard_pois_upper <- fitdist(as.numeric(hazard_data_upper[, N]), "pois", method = "mle")
    titleplot <- ggplot() + ggtitle(paste0(i," (Upper)\n", "lambda =", toString(round(hazard_pois_upper$estimate, 2)))) + 
      theme_void() + theme(plot.title = element_text(size = 30, vjust = -25, hjust = 0.4))
    print(titleplot)
    plot(hazard_freq_pois)
    parameter_estimates <- rbind(parameter_estimates, data.table(Hazard = i, Region = "Total", Upper_Lower = "Upper", Lambda = hazard_pois_upper$estimate))
  }
  
  if (nrow(hazard_data_lower) > 1){
    hazard_pois_lower <- fitdist(as.numeric(hazard_data_lower[, N]), "pois", method = "mle")
    titleplot <- ggplot() + ggtitle(paste0(i, " (Lower)\n", "lambda =", toString(round(hazard_pois_lower$estimate, 2)))) + 
      theme_void() + theme(plot.title = element_text(size = 30, vjust = -25, hjust = 0.4))
    print(titleplot)
    plot(hazard_freq_pois)
    parameter_estimates <- rbind(parameter_estimates, data.table(Hazard = i, Region = "Total", Upper_Lower = "Lower", Lambda = hazard_pois_lower$estimate))
  }
    
  
}

dev.off()



# Fitting Poisson distribution



hd[, .N, HazardEventGrouped]

# hazard <- hazard_list[8] # Hurricane

# hazard <- hazard_list[12] # Wildfire

# hazard <- hazard_list[2] # Severe Storm/ThunderStorm/Wind

# hazard <- hazard_list[5] # Flooding

hazard <- hazard_list[4] # Hail

hazard_freq <- hd[HazardEventGrouped == hazard & Year < 2010, .N, .(Year, Region)]

# hazard_freq <- hd[HazardEventGrouped == hazard, .N, .(Year, Region)] - ordinary

hazard_freq_region <- hazard_freq[Region == 1]

# Plotting frequency distribution by region

ggplot() + geom_line(data = hazard_freq, aes(x = Year, y = N, color = factor(Region)))

# Poisson

  # Fitting frequency distribution by region

hazard_freq_pois <- fitdist(as.numeric(hazard_freq_region[, N]), "pois", method = "mle")

# hazard_freq_pois <- fitdist(as.numeric(hazard_freq[, N]), "pois", method = "mle")

plot(hazard_freq_pois)

# Negative binomial

storm_freq_nb <- fitdist(as.numeric(storm_freq[, N]), "nbinom", method = "mle")

plot(storm_freq_nb)

gofstat(storm_freq_pois)


# Fitting Poisson distributions to each 

