# Fitting upper size distributions for data

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)
library(actuar)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])


# Fitting upper distribution on each of the regions

agg_gpd_params <- data.table(Region = 0, Scale = 0, Shape = 0)

hazard_data_excl_outliers <- hd[!(Key %in% c("H2828", "H2827", "W3232", 
                                             "D62", "D63", "D64", "D65", "D66", "D67",
                                              "F2702", "F2737", "F2738", "F2741", "F2746", "F2756",
                                              "H810", "H811", "H812", "H813", "H814")) &
                                 InflatedPropertyDamage > 30000000]

for (i in region_list) {

  agg_hazard_region <- hazard_data_excl_outliers[Region == i, InflatedPropertyDamage]/1000
  
  if (length(agg_hazard_region) > 1){
  
    agg_gpd <- POT::fitgpd(data = hazard_data_excl_outliers[Region == i, InflatedPropertyDamage]/1000, threshold = 30000, est="mle")
    
    agg_gpd_params <- rbind(agg_gpd_params, data.table(Region = i, Scale = agg_gpd$fitted.values[1], Shape = agg_gpd$fitted.values[2]))
    
    plot(agg_gpd)
    
  }
  
}

# Aggregate distributions on all regions

freak_hazards <- hd[Key %in% c("H2828", "H2827", "W3232", 
                                                      "D62", "D63", "D64", "D65", "D66", "D67",
                                                      "F2702", "F2737","F2738", "F2741", "F2746", "F2756",
                                                      "H810", "H811", "H812", "H813", "H814")]

freak_hazards[, Freak_Key := paste0(substr(Key, 1, 1), Year)]

freak_hazard_grouped <- freak_hazards[, .(TotInflatedPD = sum(InflatedPropertyDamage)), .(Freak_Key, Year)]

agg_gpd <- POT::fitgpd(data = freak_hazard_grouped[, TotInflatedPD]/1000, threshold = 30000, est="mle")

#plot(agg_gpd)

agg_gpd_params <- rbind(agg_gpd_params, data.table(Region = 0, Scale = agg_gpd$fitted.values[1], Shape = agg_gpd$fitted.values[2]))

agg_gpd_params <- agg_gpd_params[-1]








