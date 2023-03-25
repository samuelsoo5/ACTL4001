# Fitting lower size distributions for data

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)
library(actuar)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])


# Fitting lower distribution on each of the regions

agg_wb_params <- data.table(Region = 0, Shape = 0, Scale = 0)

hazard_data <- hd[!(Key %in% c("H2828", "H2827", "W3232", 
                                             "D62", "D63", "D64", "D65", "D66", "D67",
                                             "F2702", "F2737", "F2738", "F2741", "F2746", "F2756",
                                             "H810", "H811", "H812", "H813", "H814")) &
                                  InflatedPropertyDamage <= 1000000 & InflatedPropertyDamage > 0]


for (i in region_list) {
  
  agg_hazard_region <- hazard_data[Region == i, InflatedPropertyDamage]/1000
  
  agg_wb <- fitdist(data = agg_hazard_region, "weibull", method ="mle")
  
  agg_wb_params <- rbind(agg_wb_params, data.table(Region = i, Shape = agg_wb$estimate[1], Scale = agg_wb$estimate[2]))
  
  #plot(agg_wb)
  
}

  # Removing test value

agg_wb_params <- agg_wb_params[-1]

# Fitting middle distribution on each of the regions

mid_pr_params <- data.table(Region = 0, Scale = 0, Shape = 0)

hazard_data_mid <- hd[!(Key %in% c("H2828", "H2827", "W3232", 
                               "D62", "D63", "D64", "D65", "D66", "D67",
                               "F2702", "F2737", "F2738", "F2741", "F2746", "F2756",
                               "H810", "H811", "H812", "H813", "H814")) &
                    InflatedPropertyDamage <= 30000000 & InflatedPropertyDamage > 1000000]

for (i in region_list) {
  
  mid_hazard_region <- hazard_data_mid[Region == i, InflatedPropertyDamage]/1000
  
  mid_pr <- POT::fitgpd(data = mid_hazard_region, threshold = 1000, est ="mle")
  
  mid_pr_params <- rbind(mid_pr_params, data.table(Region = i, Scale = mid_pr$fitted.values[1], Shape = mid_pr$fitted.values[2]))
  
  #plot(mid_pr)
  
}

  # Removing test value

mid_pr_params <- mid_pr_params[-1]
