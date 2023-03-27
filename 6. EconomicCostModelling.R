library(readr)
library(data.table)
library(ggplot2)


####################################################

# Basic script sor Economic Cost Modelling

####################################################


# Import All Necessary Data

fl1 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_1.csv"
fl2 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_2.csv"
fl3 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_3.csv"
fl4 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_4.csv"
fl5 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_5.csv"
fl6 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Lower_Region_6.csv"
fm1 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_1.csv"
fm2 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_2.csv"
fm3 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_3.csv"
fm4 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_4.csv"
fm5 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_5.csv"
fm6 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Middle_Region_6.csv"
fu1 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Upper_Region_1.csv"
fu2 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Upper_Region_2.csv"
fu3 = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Upper_Region_3.csv"
ffreak = "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Frequency_Freak.csv"

sl1 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_1.csv"
sl2 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_2.csv"
sl3 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_3.csv"
sl4 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_4.csv"
sl5 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_5.csv"
sl6 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Lower_Region_6.csv"
sm1 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_1.csv"
sm2 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_2.csv"
sm3 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_3.csv"
sm4 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_4.csv"
sm5 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_5.csv"
sm6 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Middle_Region_6.csv"
su1 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Upper_Region_1.csv"
su2 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Upper_Region_2.csv"
su3 =  "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Upper_Region_3.csv"

sfreak =   "https://raw.githubusercontent.com/samuelsoo5/ACTL4001/main/Simulated%20values/Severity_Upper_Region_0.csv"


Regions <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3, "total")
Severity <- c("L", "L","L", "L","L", "L","M", "M","M", "M","M", "M", "U", "U", "U", "F")

# freq tables
dt_list_f <- list()
csv_urls <- c(fl1,fl2,fl3,fl4,fl5,fl6,fm1,fm2,fm3,fm4,fm5,fm6,fu1,fu2,fu3, ffreak)

for (url in csv_urls) {
  dt <- fread(url)[c(1:1000), c(-1, -81)] # Can change this to include all sims
  # Extract filename srom URL
  filename <- basename(url)
  dt_list_f[[filename]] <- dt
}



# severity Tables
dt_list_s <- list()
csv_urls <- c(sl1,sl2,sl3,sl4,sl5,sl6,sm1,sm2,sm3,sm4,sm5,sm6,su1,su2,su3, sfreak)

for (url in csv_urls) {
  dt <- fread(url)[c(1:1000), c(-1, -81)]
  # Extract filename srom URL
  filename <- basename(url)
  dt_list_s[[filename]] <- dt
}




# Import Baseline Information


recovery <- data.table(Severity = c("L", "M", "U", "F"),
                       time_noprg = c(1/12, 1/2, 1, 2),
                       time_prg = c(1/24, 1/4, 1/2, 1.5))

rebuild_multi <- data.table(Severity = c("L", "M", "U", "F"),
                            multiplier = c(1,1.25,1.5, 1.5))
replacing <- c(0.4,0.75)

scenarios <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/Scenarios.csv")
scenarios <- lapply(scenarios, as.vector)[-1]
scenarionames <- c("SSP1", "SSP2", "SSP3", "SSP4")

baseline <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/Baselineinformation.csv")

voluntary <- c(0.20, 0.1,0.15,0.25,0.3)
fatality <- 0.5727 # vector of mean times by severity level (aggregate by threshold)
injury <- 2.046

medianage <- c(31,37)
medianfood <- 3.54*1.321
retirement <- 65



inflation <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/inflation.csv")
inflation[, c(2:101)] <- inflation[, c(2:101)]/100+1

rf1 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/rf1.csv")
rf10 <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/rf10.csv")
rf10[, c(2:101)] <- rf10[, c(2:101)]/100+1

GDPgrowth <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/GDPgrowth.csv")
GDPgrowth <- lapply(GDPgrowth, as.vector)[-1]
totalGDP <- sum(baseline$GDP2020_1000)

populationgrowth <- fread("C:/Users/PC/Documents/GitHub/ACTL4001/Data/populationgrowth.csv")
populationgrowth <- lapply(populationgrowth, as.vector)[-1]


###########################################

# Economic Cost Modelling - Without Program


###########################################

scenario_list <- list()

# Choose Inflation & Interest rate sim --> nsim[i]

nsim <- seq(2,101,1)
inflationsim <- c(1.0384, inflation[, c(1, ..nsim[5])][, cuminf := cumprod(.SD[[2]])]$cuminf)
discountsim <- c(1.0166, rf10[, c(1, ..nsim[5])][, cumdc := cumprod(.SD[[2]])]$cumdc)


# Choose Scenario -SSP1 = 1, SSP2 = 2, SSP3 = 3, SSP5 = 4
for(s in 1:4){
  
  freqx <- unlist(scenarios[s])
  g <- as.numeric(unlist(GDPgrowth[s]))
  p <- as.numeric(unlist(populationgrowth[s]))
  
  # Recovery Times without Program
  rec_pro <- recovery[, c("Severity", "time_noprg")]
  
  
  #total_cost <- data.table(matrix(ncol = 6, nrow = 0, dimnames = list(NULL, c("Year", "GDP", "Total Property Damage", "Total Economic Cost", "Total Injuries", "Total Fatalities"))))
  
  
  # Loop for simulations
  
  aggregate_table <- data.table(Sim_No = 0,
                          Year = 0,
                          GDP = 0,
                          TotalPropertyDamage = 0,
                          TotalEconomicCost = 0,
                          TotalInj = 0,
                          TotalFat = 0)
  
  for(sim in 1:1000){
    print(paste0(s, ", ", sim))
    total_cost <- rep(0, each = 80)
    total_damage <- rep(0, each = 80)
    total_inj <- rep(0, each = 80)
    total_fat <- rep(0, each = 80)
    # Loop through each region and severity level
    for(i in 1:length(dt_list_f)){
      
      dtf <- dt_list_f[[i]]
      dts <- dt_list_s[[i]]
      r <- Regions[i]
      sev <- Severity[i]
      
      # Multiply Freq and Sev to get total damage for region
      dtf <- data.table(t(dtf))
      dts <- data.table(t(dts))
      
      
      # Run for 1000 Simulations (unfinished, only first simulation rn)
      total <- data.table(Year = seq(2021,2100,1),
                          GDP = baseline[Regions == r, GDP2020_1000]*g,
                          Population = baseline[Regions == r, Population]*p,
                          total = as.numeric(unlist(dtf[, ..sim] * dts[, ..sim])) * 1000 * inflationsim * freqx,
                          housevalue = baseline[Regions == r, MedianHouse] * inflationsim, 
                          pphouse = baseline[Regions == r, PpHouse],
                          injuries = as.numeric(unlist(dtf[, ..sim])) * freqx * injury,
                          fatalities = as.numeric(unlist(dtf[, ..sim])) * freqx * fatality)
      total <- total[, no_houses := total/housevalue][, no_pp := no_houses*pphouse][, GDPpercapita := GDP/Population]
      
      
      #### Calculate Region Specific costs ####
      
      # Temp Housing
      total <- total[, TempHousing := no_pp*inflationsim*as.numeric(baseline[Regions == r, TemporaryHousing]*
                                                                      rec_pro[Severity == sev, 2])*12]
      
      # Lost productivity from time taken to recover
      total <- total[, Lostprod := no_pp*as.numeric(baseline[Regions == r, LabourForce]
                                                    *baseline[Regions == r, EmploymentRate]
                                                    *recovery[Severity == sev, 2]
                                                    *GDPpercapita)*1000]
      
      # Lost productivity from Fatalities
      total <- total[, cumfat :=  frollsum(fatalities, 34, align = "right", fill = NA)]
      total[1:33, c("cumfat")] <- cumsum(total$fatalities[1:33])
      total <- total[, FatalityCost := cumfat*GDPpercapita*1000]
      
      
      # Food Cost
      total <- total[, Foodcost := no_pp*inflationsim*medianfood*as.numeric(recovery[Severity == sev, 2])*365]
      
      # Healthcare Cost / Cost of Injury
      total <- total[, InjuryCost := no_pp*inflationsim*as.numeric(baseline[Regions == r, HealthcareCost])]
      
      # Cost of Rebuilding
      total <- total[, Rebuilding := total*as.numeric(rebuild_multi[Severity == sev, multiplier])]
      
      # Cost of replacing household items (40%, can be changed to 75% with 2)
      total <- total[, Replacing1 := Rebuilding*replacing[1]]
      total <- total[, Replacing2 := Rebuilding*replacing[2]]
      
      # Other costs not considered: 
      # cost to transportation network because of road and warehouse destruction
      # cost of lost output of retailers
      
      # Total Economic cost for this region and severity level
      total <- total[, EconomicCost := TempHousing + Lostprod + FatalityCost + Foodcost + InjuryCost +
                       Rebuilding + Replacing2][, EconomicCost := (round(EconomicCost,0))]
      
      total_cost <- total_cost + total$EconomicCost     
      total_damage <- total_damage + total$total
      total_inj <- total_inj + total$injuries
      total_fat <- total_fat + total$fatalities
      
      
    }
  
  aggregate <- data.table(Sim_No = sim,
                          Year = seq(2021,2100,1),
                          GDP = totalGDP*g,
                          TotalPropertyDamage = total_damage,
                          TotalEconomicCost = total_cost,
                          TotalInj = total_inj,
                          TotalFat = total_fat)
  
  aggregate_table <- rbind(aggregate_table, aggregate)
  
  total_cost <- 0 
   
  
  }
  
  aggregate_table <- aggregate_table[, I := ifelse(GDP*100 < TotalEconomicCost, 1, 0)]
  tablename <- scenarionames[s]
  scenario_list[[tablename]] <- aggregate_table
  
}

# Check how many simulations are over the 10% GDP line
degcertainty <- scenario_list[[1]][, .(check = sum(I)), Sim_No]
table(degcertainty$check)


# Scenario 1
ggplot(data = scenario_list[[1]][Sim_No == 20,]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

ggplot(data = aggregate_table[Sim_No == 100,]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 2
ggplot(data = scenario_list[[2]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 3
ggplot(data = scenario_list[[3]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 4
ggplot(data = scenario_list[[4]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))





###########################################

# Economic Cost Modelling - With Program

###########################################

scenario_list2 <- list()

# Choose Inflation & Interest rate sim --> nsim[i]

nsim <- seq(2,101,1)
inflationsim <- c(1.0384, inflation[, c(1, ..nsim[5])][, cuminf := cumprod(.SD[[2]])]$cuminf)
discountsim <- c(1.0166, rf10[, c(1, ..nsim[5])][, cumdc := cumprod(.SD[[2]])]$cumdc)


# Choose Scenario -SSP1 = 1, SSP2 = 2, SSP3 = 3, SSP5 = 4
for(s in 1:4){
  
  freqx <- unlist(scenarios[s])
  g <- as.numeric(unlist(GDPgrowth[s]))
  p <- as.numeric(unlist(populationgrowth[s]))
  
  # Recovery Times without Program
  rec_pro <- recovery[, c("Severity", "time_prg")]
  
  
  #total_cost <- data.table(matrix(ncol = 6, nrow = 0, dimnames = list(NULL, c("Year", "GDP", "Total Property Damage", "Total Economic Cost", "Total Injuries", "Total Fatalities"))))
  
  
  # Loop for simulations
  
  aggregate_table <- data.table(Sim_No = 0,
                                Year = 0,
                                GDP = 0,
                                TotalPropertyDamage = 0,
                                TotalEconomicCost = 0,
                                TotalInj = 0,
                                TotalFat = 0)
  
  for(sim in 1:1000){
    print(sim)
    total_temphousing <- rep(0, each = 80)
    total_LostProd <- trep(0, each = 80)
    total_FoodCost <- rep(0, each = 80)
    total_InjuryCost <- rep(0, each = 80)
    total_Rebuilding <- rep(0, each = 80)
    total_Replacing <- rep(0, each = 80)
    total_cost <- rep(0, each = 80)
    total_damage <- rep(0, each = 80)
    total_inj <- rep(0, each = 80)
    total_fat <- rep(0, each = 80)
    # Loop through each region and severity level
    for(i in 1:length(dt_list_f)){
      
      dtf <- dt_list_f[[i]]
      dts <- dt_list_s[[i]]
      r <- Regions[i]
      sev <- Severity[i]
      
      # Multiply Freq and Sev to get total damage for region
      dtf <- data.table(t(dtf))
      dts <- data.table(t(dts))
      
      
      # Run for 1000 Simulations (unfinished, only first simulation rn)
      total <- data.table(Year = seq(2021,2100,1),
                          GDP = baseline[Regions == r, GDP2020_1000]*g,
                          Population = baseline[Regions == r, Population]*p,
                          total = as.numeric(unlist(dtf[, ..sim] * dts[, ..sim])) * 1000 * inflationsim * freqx,
                          housevalue = baseline[Regions == r, MedianHouse] * inflationsim, 
                          pphouse = baseline[Regions == r, PpHouse],
                          injuries = as.numeric(unlist(dtf[, ..sim])) * freqx * injury,
                          fatalities = as.numeric(unlist(dtf[, ..sim])) * freqx * fatality)
      total <- total[, no_houses := total/housevalue][, no_pp := no_houses*pphouse][, GDPpercapita := GDP/Population]
      
      
      #### Calculate Region Specific costs ####
      
      # Temp Housing
      total <- total[, TempHousing := no_pp*inflationsim*as.numeric(baseline[Regions == r, TemporaryHousing]*
                                                                      rec_pro[Severity == sev, 2])*12]
      
      # Lost productivity from time taken to recover
      total <- total[, Lostprod := no_pp*as.numeric(baseline[Regions == r, LabourForce]
                                                    *baseline[Regions == r, EmploymentRate]
                                                    *recovery[Severity == sev, 2]
                                                    *GDPpercapita)*1000]
      
      # Lost productivity from Fatalities
      total <- total[, cumfat :=  frollsum(fatalities, 34, align = "right", fill = NA)]
      total[1:33, c("cumfat")] <- cumsum(total$fatalities[1:33])
      total <- total[, FatalityCost := cumfat*GDPpercapita*1000]
      
      
      # Food Cost
      total <- total[, Foodcost := no_pp*inflationsim*medianfood*as.numeric(recovery[Severity == sev, 2])*365]
      
      # Healthcare Cost / Cost of Injury
      total <- total[, InjuryCost := no_pp*inflationsim*as.numeric(baseline[Regions == r, HealthcareCost])]
      
      # Cost of Rebuilding
      total <- total[, Rebuilding := total*as.numeric(rebuild_multi[Severity == sev, multiplier])]
      
      # Cost of replacing household items (40%, can be changed to 75% with 2)
      total <- total[, Replacing1 := Rebuilding*replacing[1]]
      total <- total[, Replacing2 := Rebuilding*replacing[2]]
      
      # Other costs not considered: 
      # cost to transportation network because of road and warehouse destruction
      # cost of lost output of retailers
      
      # Total Economic cost for this region and severity level
      total <- total[, EconomicCost := TempHousing + Lostprod + FatalityCost + Foodcost + InjuryCost +
                       Rebuilding + Replacing1][, EconomicCost := (round(EconomicCost,0))]
      
      total_temphousing <- total_temphousing + total$TempHousing
      total_LostProd <- total_LostProd + total$Lostprod
      total_FoodCost <- total_FoodCost + total$Foodcost
      total_InjuryCost <- total_InjuryCost + total$InjuryCost
      total_Rebuilding <- total_Rebuilding + total$Rebuilding
      total_Replacing <- total_Replacing + total$Replacing1
      total_cost <- total_cost + total$EconomicCost     
      total_damage <- total_damage + total$total
      total_inj <- total_inj + total$injuries
      total_fat <- total_fat + total$fatalities
      
      
    }
    
    aggregate <- data.table(Sim_No = sim,
                            Year = seq(2021,2100,1),
                            GDP = totalGDP*g,
                            TotalPropertyDamage = total_damage,
                            TotalEconomicCost = total_cost,
                            TotalInj = total_inj,
                            TotalFat = total_fat)
    
    aggregate_table <- rbind(aggregate_table, aggregate)
    
    total_cost <- 0 
    
    
  }
  
  aggregate_table <- aggregate_table[, I := ifelse(GDP*100 < TotalEconomicCost, 1, 0)]
  tablename <- scenarionames[s]
  scenario_list2[[tablename]] <- aggregate_table
  
}

# Check how many simulations are over the 10% GDP line
degcertainty <- scenario_list2[[1]][, .(check = sum(I)), Sim_No]
table(degcertainty$check)

# aggregate_table <- aggregate_table[, I := ifelse(GDP*100 < TotalEconomicCost, 1, 0)]
# simcheck <- aggregate_table[, .(check = sum(I)), Sim_No][, check := ifelse(check > 0, 1, 0)]
# table(simcheck$check)

aggregate_table <- 

# Scenario 1
ggplot(data = scenario_list2[[1]][Sim_No == 167,]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

ggplot(data = aggregate_table[Sim_No == 1,]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

ggplot(data = aggregate_table[Sim_No == 1,]) +
  geom_bar(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 2
ggplot(data = scenario_list2[[2]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 3
ggplot(data = scenario_list2[[3]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))

# Scenario 4
ggplot(data = scenario_list2[[4]]) +
  geom_line(aes(x = Year, y = TotalEconomicCost, color = "Economic Cost")) +
  geom_line(aes(x = Year, y = TotalPropertyDamage, color = "Property Damage")) +  geom_line(aes(x = Year, y = GDP*100, color = "GDP"))



