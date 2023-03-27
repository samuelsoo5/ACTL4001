# Random simulations for each of the distributions

library(ggplot2)
library(fitdistrplus)
library(data.table)
library(POT)
library(actuar)

# Setting seed

no_sims <- 10000

no_years <- length(seq(2020,2100))

# Loading data

hd <- fread("C:/Users/PC/Documents/Uni/4001/Data/Hazard data.csv")

region_list <- unique(hd[, Region])

# Severity simulations

source("C:/Users/PC/Documents/GitHub/ACTL4001/1. Fitting upper severity and outlier pareto distributions.R")

#### Simulating upper and freak distribution severities ####

for (j in unique(agg_gpd_params[, Region])){

    # Creating random matrix for probabilities
  
  upper_rand <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
    # Mapping these probabilities to observations
  
  sim_values_upper<- matrix(sapply(upper_rand, POT::qgpd, scale = agg_gpd_params[Region == j, Scale], shape = agg_gpd_params[Region == j, Shape]),
                            nrow = no_sims)
  
    # Binding these columns to a matrix
  
  dt_upper <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_upper <- cbind(dt_upper, sim_values_upper[, i])
    
  }
  
    # Setting column names
  
  colnames(dt_upper) <- c("Index", paste(seq(2020,2100), sep = " "))
  
    # Saving file
  
  fwrite(dt_upper, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Upper_Region_",j,".csv"))

}

#### Simulating lower distribution severities ####

source("C:/Users/PC/Documents/GitHub/ACTL4001/2. Fitting lower and middle severity distributions.R")

for (j in unique(agg_wb_params[, Region])){
  
  # Creating random matrix for probabilities
  
  lower_rand <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
  # Mapping these probabilities to observations
  
  sim_values_lower<- matrix(sapply(lower_rand, qweibull, shape = agg_wb_params[Region == j, Shape], scale = agg_wb_params[Region == j, Scale]),
                            nrow = no_sims)
  
  # Binding these columns to a matrix
  
  dt_lower <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_lower <- cbind(dt_lower, sim_values_lower[, i])
    
  }
  
  # Setting column names
  
  colnames(dt_lower) <- c("Index", paste(seq(2020,2100), sep = " "))
  
  # Saving file
  
  fwrite(dt_lower, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Lower_Region_",j,".csv"))
  
}

#### Simulating middle distribution severities ####

for (j in unique(mid_pr_params[, Region])){
  
  # Creating random matrix for probabilities
  
  middle_rand <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
  # Mapping these probabilities to observations
  
  sim_values_middle<- matrix(sapply(middle_rand, POT::qgpd, scale = mid_pr_params[Region == j, Scale], shape = mid_pr_params[Region == j, Shape]),
                            nrow = no_sims)
  
  # Binding these columns to a matrix
  
  dt_middle <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_middle <- cbind(dt_middle, sim_values_middle[, i])
    
  }
  
  # Setting column names
  
  colnames(dt_middle) <- c("Index", paste(seq(2020,2100), sep = " "))
  
  # Saving file
  
  fwrite(dt_middle, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Severity_Middle_Region_",j,".csv"))
  
}


#### Simulating lower distribution frequencies ####

source("C:/Users/PC/Documents/GitHub/ACTL4001/3. Fitting aggregate and region specific poisson distribution.R")

for (j in unique(nb_params_lower[, Region])){
  
  print(j)
  
  # Creating random matrix for probabilities
  
  lower_rand_freq <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
  # Mapping these probabilities to observations
  
  sim_values_low_freq<- matrix(sapply(lower_rand_freq, qnbinom, size = nb_params_lower[Region == j, Size], prob = nb_params_lower[Region == j, Prob]),
                             nrow = no_sims)
  
  # Binding these columns to a matrix
  
  dt_low_freq <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_low_freq <- cbind(dt_low_freq, sim_values_low_freq[, i])
    
  }
  
  # Setting column names
  
  colnames(dt_low_freq) <- c("Index", paste(seq(2020,2100), sep = " "))
  
  # Saving file
  
  fwrite(dt_low_freq, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Lower_Region_",j,".csv"))
  
}

#### Simulating middle distribution frequencies ####

for (j in unique(nb_params_mid[, Region])){
  
  print(j)
  
  # Creating random matrix for probabilities
  
  middle_rand_freq <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
  # Mapping these probabilities to observations
  
  sim_values_middle_freq<- matrix(sapply(middle_rand_freq, qnbinom, size = nb_params_mid[Region == j, Size], prob = nb_params_mid[Region == j, Prob]),
                               nrow = no_sims)
  
  # Binding these columns to a matrix
  
  dt_mid_freq <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_mid_freq <- cbind(dt_mid_freq, sim_values_middle_freq[, i])
    
  }
  
  # Setting column names
  
  colnames(dt_mid_freq) <- c("Index", paste(seq(2020,2100), sep = " "))
  
  # Saving file
  
  fwrite(dt_mid_freq, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Middle_Region_",j,".csv"))
  
}

#### Simulating upper distribution frequencies ####

for (j in unique(pois_params_upper[, Region])){
  
  print(j)
  
  # Creating random matrix for probabilities
  
  upper_rand_freq <- matrix(runif(no_years*no_sims), nrow = no_sims)
  
  # Mapping these probabilities to observations
  
  sim_values_upper_freq<- matrix(sapply(upper_rand_freq, qpois, lambda = pois_params_upper[Region == j, Lambda]),
                                 nrow = no_sims)
  
  # Binding these columns to a matrix
  
  dt_upper_freq <- data.table(Index = seq(1, no_sims))
  
  for (i in 1:no_years) {
    
    dt_upper_freq <- cbind(dt_upper_freq, sim_values_upper_freq[, i])
    
  }
  
  # Setting column names
  
  colnames(dt_upper_freq) <- c("Index", paste(seq(2020,2100), sep = " "))
  
  # Saving file
  
  fwrite(dt_upper_freq, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Upper_Region_",j,".csv"))
  
}

#### Simulating freak distribution frequencies ####

# Creating random matrix for probabilities

freak_rand_freq <- matrix(runif(no_years*no_sims), nrow = no_sims)

# Mapping these probabilities to observations

sim_values_freak_freq <- matrix(sapply(freak_rand_freq, qpois, lambda = pois_params_freak[, Lambda]),
                               nrow = no_sims)

# Binding these columns to a matrix

dt_freak_freq <- data.table(Index = seq(1, no_sims))

for (i in 1:no_years) {
  
  dt_freak_freq <- cbind(dt_freak_freq, sim_values_freak_freq[, i])
  
}

# Setting column names

colnames(dt_freak_freq) <- c("Index", paste(seq(2020,2100), sep = " "))

# Saving file

fwrite(dt_freak_freq, paste0("C:/Users/PC/Documents/GitHub/ACTL4001/Simulated values/Frequency_Freak",".csv"))









