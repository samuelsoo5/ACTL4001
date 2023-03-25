# Combining output values and finding scaling factors

library(data.table)

# Reading in data

  # Severity

sev_freak <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Upper_Region_0.csv")

sev_up_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Upper_Region_1.csv")
sev_up_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Upper_Region_2.csv")
sev_up_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Upper_Region_3.csv")

sev_mid_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_1.csv")
sev_mid_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_2.csv")
sev_mid_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_3.csv")
sev_mid_4 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_4.csv")
sev_mid_5 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_5.csv")
sev_mid_6 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_6.csv")

sev_low_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_1.csv")
sev_low_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_2.csv")
sev_low_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_3.csv")
sev_low_4 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_4.csv")
sev_low_5 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_5.csv")
sev_low_6 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_6.csv")

  # Frequency

freq_freak <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Freak.csv")

freq_up_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Upper_Region_1.csv")
freq_up_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Upper_Region_2.csv")
freq_up_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Upper_Region_3.csv")

freq_mid_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_1.csv")
freq_mid_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_2.csv")
freq_mid_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_3.csv")
freq_mid_4 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_4.csv")
freq_mid_5 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_5.csv")
freq_mid_6 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_6.csv")

freq_low_1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_1.csv")
freq_low_2 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_2.csv")
freq_low_3 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_3.csv")
freq_low_4 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_4.csv")
freq_low_5 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_5.csv")
freq_low_6 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_6.csv")

# Calculating total costs

agg_freak <- data.table(sapply(sev_freak[, -1] * freq_freak[, -1], mean))

agg_up_1 <- data.table(sapply(sev_up_1[, -1] * freq_up_1[, -1], mean))
agg_up_2 <- data.table(sapply(sev_up_2[, -1] * freq_up_2[, -1], mean))
agg_up_3 <- data.table(sapply(sev_up_3[, -1] * freq_up_3[, -1], mean))

agg_mid_1 <- data.table(sapply(sev_mid_1[, -1] * freq_mid_1[, -1], mean))
agg_mid_2 <- data.table(sapply(sev_mid_2[, -1] * freq_mid_2[, -1], mean))
agg_mid_3 <- data.table(sapply(sev_mid_3[, -1] * freq_mid_3[, -1], mean))
agg_mid_4 <- data.table(sapply(sev_mid_4[, -1] * freq_mid_4[, -1], mean))
agg_mid_5 <- data.table(sapply(sev_mid_5[, -1] * freq_mid_5[, -1], mean))
agg_mid_6 <- data.table(sapply(sev_mid_6[, -1] * freq_mid_6[, -1], mean))

agg_low_1 <- data.table(sapply(sev_low_1[, -1] * freq_low_1[, -1], mean))
agg_low_2 <- data.table(sapply(sev_low_2[, -1] * freq_low_2[, -1], mean))
agg_low_3 <- data.table(sapply(sev_low_3[, -1] * freq_low_3[, -1], mean))
agg_low_4 <- data.table(sapply(sev_low_4[, -1] * freq_low_4[, -1], mean))
agg_low_5 <- data.table(sapply(sev_low_5[, -1] * freq_low_5[, -1], mean))
agg_low_6 <- data.table(sapply(sev_low_6[, -1] * freq_low_6[, -1], mean))

# Adding up costs by region

agg_region_1 <- agg_up_1 + agg_mid_1 + agg_low_1 + agg_freak/6
agg_region_2 <- agg_up_2 + agg_mid_2 + agg_low_2 + agg_freak/6
agg_region_3 <- agg_up_3 + agg_mid_3 + agg_low_3 + agg_freak/6
agg_region_4 <- agg_mid_4 + agg_low_4 + agg_freak/6
agg_region_5 <- agg_mid_5 + agg_low_5 + agg_freak/6
agg_region_6 <- agg_mid_6 + agg_low_6 + agg_freak/6

# Scaling each region by 2020 total property value, giving proportion of area damaged

property_values<- c(362.27965, 314.54415, 304.2482, 188.4346, 208.8136, 249.0995)
households <- c(2376180, 1634628, 1865736, 403548, 500448, 110052)

total_property_value <- property_values * households

agg_region_1_scaled <- agg_region_1/total_property_value[1]
agg_region_2_scaled <- agg_region_2/total_property_value[2]
agg_region_3_scaled <- agg_region_3/total_property_value[3]
agg_region_4_scaled <- agg_region_4/total_property_value[4]
agg_region_5_scaled <- agg_region_5/total_property_value[5]
agg_region_6_scaled <- agg_region_6/total_property_value[6]


# Joinng onto one data.table

agg_costs <- data.table(Year = seq(2020, 2100))

agg_costs_total <- cbind(agg_costs, agg_region_1, agg_region_2, agg_region_3, agg_region_4, agg_region_5, agg_region_6)
agg_costs_scaled <- cbind(agg_costs, agg_region_1_scaled, agg_region_2_scaled, agg_region_3_scaled, agg_region_4_scaled, agg_region_5_scaled, agg_region_6_scaled)

colnames(agg_costs_total) <- c("Year", "Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6")
colnames(agg_costs_scaled) <- c("Year", "Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6")

fwrite(agg_costs_scaled, "C:/Users/PC/Documents/GitHub/ACTL4001/Output/agg_costs_scaled.csv")
fwrite(agg_costs_total, "C:/Users/PC/Documents/GitHub/ACTL4001/Output/agg_costs_total.csv")






