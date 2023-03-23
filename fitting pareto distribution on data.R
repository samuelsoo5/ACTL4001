library(extRemes)
library(evir)
#library(evmix) # gpd density functions
library(goft) #gp_fit
library(POT) #fit gpd
library(tea) #
library(ggplot2)
library(data.table)

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

hazard_list <- unique(hd[, HazardEventGrouped])
region_list <- unique(hd[, Region])


# Fitting on aggregate

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/gpd_threshhold_total.pdf")

for (i in hazard_list) {
  
  print(i)
  
    
  hazard_data <- as.numeric(hd[HazardEventGrouped == i & PropertyDamage > 0, PropertyDamage])
  
  if (length(hazard_data) >= 5) {
    titleplot <- ggplot() + ggtitle(paste0(i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
    print(titleplot)
    POT::tcplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=0.9)/1000))
    mrlplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=1 - 1/length(hazard_data))/1000),col=c("green","black","green"), nt= length(hazard_data))
    gplot <- tea::ggplot(as.numeric(hazard_data)/1000)
    
    intersection_points <- sort(hazard_data)[gplot$k0]
    
    endplot <- ggplot() + ggtitle(paste0("Intersection points:\n", paste(sort(hazard_data)[gplot$k0], collapse = " "))) + 
      theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
    print(endplot)
  }
    
  
}

dev.off()






# Fitting on each individual region

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/gpd_threshhold.pdf")

for (i in hazard_list) {
  
  print(i)
  
  for (j in region_list){

    hazard_data <- as.numeric(hd[HazardEventGrouped == i & PropertyDamage > 0 & Region == j, PropertyDamage])
    
    if (length(hazard_data) > 10){
      titleplot <- ggplot() + ggtitle(paste0(i, " Region ", j)) + 
        theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
      print(titleplot)
      tcplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=1 - 1/length(hazard_data))/1000))
      mrlplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=1 - 1/length(hazard_data))/1000),col=c("green","black","green"), nt=length(hazard_data))
      tea::ggplot(as.numeric(hazard_data)/1000)
    }
    
  }
  
}

dev.off()


# Hazard input

hazard_list <- unique(hd[, HazardEventGrouped])

#hazard <- hazard_list[8] # hurricane

hazard <- hazard_list[12] # wildfire

# Plotting a mean residual life plot

hazard_data = hd[HazardEventGrouped == hazard
                 & PropertyDamage > 0]

hazard_pd = as.numeric(hd[HazardEventGrouped == hazard
                                 & PropertyDamage > 0, PropertyDamage])

mrlplot(hazard_pd, nint = 500)

ggplot2::ggplot() + geom_point(data = hazard_data, aes(x = factor(Year), y = as.numeric(PropertyDamage)))

# Parameter estimates

thresh <- 100

probs <- 1/thresh * seq(1, thresh-1)

quantiles <- quantile(hazard_pd, probs)

hazard_sample <- hd[HazardEventGrouped == hazard, PropertyDamage]

# Defining blank arrays for scale and shape

scale_vec <- rep(0, length(quantiles))

shape_vec <- rep(0, length(quantiles))

# Fitting models on various threshholds

for (i in 1:length(quantiles)) {

    # Fitting model
  
  #storm_sample <- hd[HazardEventGrouped == "Severe Storm/ThunderStorm/Wind" &
                     #PropertyDamage > quantiles[i], PropertyDamage]
  
  hazard_sample_gpd <- fitgpd(data = as.numeric(hazard_sample), threshold = quantiles[i]) # using pot
  
  #hazard_sample_gpd <- fitdist(as.numeric(hazard_sample), dgpd, method = "mle", 
                               #start = list(sigmau = 1, xi = 0))
  
  #storm_sample_gpd <- gp_fit(as.numeric(storm_sample), method = "amle")
  
  shape_vec[i] <- hazard_sample_gpd$fitted.values[1]
  
  scale_vec[i] <- hazard_sample_gpd$fitted.values[2]
  
}

plot(quantiles, scale_vec)

plot(quantiles, shape_vec)

# Gerstengarbe Plot

hazard_sample <- hd[HazardEventGrouped == hazard, PropertyDamage]

gplot <- tea::ggplot(as.numeric(hazard_sample))

sort(hazard_sample)[gplot$k0]

plot(hazard_sample_gpd)

events0 <- clust(ardieres, u = 1.5, tim.cond = 8/365, clust.max = TRUE)
par(mfrow=c(2,2))
mrlplot(events0[,"obs"])
abline( v = 6, col = "green")
diplot(events0)
abline( v = 6, col = "green")
tcplot(events0[,"obs"], which = 1)
abline( v = 6, col = "green")
tcplot(events0[,"obs"], which = 2)
abline( v = 6, col = "green")


