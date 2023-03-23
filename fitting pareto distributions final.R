library(POT) #fitgpd
library(ggplot2)
library(data.table)

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

hazard_list <- unique(hd[, HazardEventGrouped])
region_list <- unique(hd[, Region])

# Winter Weather

hazard <- hazard_list[1]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

ww_fit <- fitgpd(data = hazard_data / 1000, threshold = 10000)

plot(ww_fit)

# Severe Storm/ThunderStorm/Wind

hazard <- hazard_list[2]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

sstw_fit <- fitgpd(data = hazard_data / 1000, threshold = 150)

plot(sstw_fit)

# Lightning

hazard <- hazard_list[3]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

light_fit <- fitgpd(data = hazard_data / 1000, threshold = 8000)

plot(light_fit)

# Hail

hazard <- hazard_list[4]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

hail_fit <- fitgpd(data = hazard_data / 1000, threshold = 100)

plot(hail_fit)

# Flooding

hazard <- hazard_list[5]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

flood_fit <- fitgpd(data = hazard_data / 1000, threshold = 25000)

plot(flood_fit)

# Heat

hazard <- hazard_list[6]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

heat_fit <- fitgpd(data = hazard_data / 1000, threshold = 1.743)

plot(heat_fit)

# Wind

hazard <- hazard_list[7]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

wind_fit <- fitgpd(data = hazard_data / 1000, threshold = 2000)

plot(wind_fit)

# Hurricane

hazard <- hazard_list[8]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

hurricane_fit <- fitgpd(data = hazard_data / 1000, threshold = 80000)

plot(hurricane_fit)

# Severe Storm/ThunderStorm

hazard <- hazard_list[9]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

sst_fit <- fitgpd(data = hazard_data / 1000, threshold = 190)

plot(sst_fit)

# Drought

hazard <- hazard_list[10]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

drought_fit <- fitgpd(data = hazard_data / 1000, threshold = 10000)

plot(drought_fit)

# Drought

hazard <- hazard_list[10]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

drought_fit <- fitgpd(data = hazard_data / 1000, threshold = 10000)

plot(drought_fit)

# Tornado

hazard <- hazard_list[11]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

torn_fit <- fitgpd(data = hazard_data / 1000, threshold = 30000)

plot(torn_fit)

# Wildfire

hazard <- hazard_list[12]

hazard_data <- hd[PropertyDamage > 0 & HazardEventGrouped == hazard, PropertyDamage]

wild_fit <- fitgpd(data = hazard_data / 1000, threshold = 20000)

plot(wild_fit)




