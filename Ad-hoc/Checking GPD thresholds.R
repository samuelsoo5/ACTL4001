# Checking GPD thresholds

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)
library(actuar)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])


# Checking GPD on aggregate

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/gpd_threshhold_aggregate.pdf")

hazard_data <- as.numeric(hd[InflatedPropertyDamage > 0, InflatedPropertyDamage])

POT::tcplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=0.995)/1000))

mrlplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=0.995)/1000), col=c("green","black","green"), nt= length(hazard_data))

gplot <- tea::ggplot(as.numeric(hazard_data)/1000)

intersection_points <- sort(hazard_data)[gplot$k0]

print(intersection_points)

dev.off()



# Checking GPD

pdf("C:/Users/PC/Documents/Uni/4001/Analysis/gpd_threshhold_by_region.pdf")

for (i in region_list) {
  
  print(paste0("Region ",i))
  
  titleplot <- ggplot() + ggtitle(paste0("Region ",i)) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(titleplot)
  
  hazard_data <- as.numeric(hd[Region == i & InflatedPropertyDamage > 0, InflatedPropertyDamage])
  
  POT::tcplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=0.995)/1000))
  
  mrlplot(hazard_data/1000, u.range=c(0,quantile(hazard_data, probs=0.995)/1000), col=c("green","black","green"), nt= length(hazard_data))
  
  gplot <- tea::ggplot(as.numeric(hazard_data)/1000)
  
  intersection_points <- sort(hazard_data)[gplot$k0]
  
  endplot <- ggplot() + ggtitle(paste0("Intersection points:\n", paste(sort(hazard_data)[gplot$k0], collapse = " "))) + 
    theme_void() + theme(plot.title = element_text(size = 20, vjust = -35, hjust = 0.5))
  print(endplot)
  
}

dev.off()