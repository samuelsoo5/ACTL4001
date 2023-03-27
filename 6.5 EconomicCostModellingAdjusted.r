
freqx <- unlist(scenarios[s])
g <- as.numeric(unlist(GDPgrowth[s]))
p <- as.numeric(unlist(populationgrowth[s]))

# Frequency tables
dt_list_f

# Severity tables
dt_list_s

# Lower_agg

lower_frequency <- data.table()

for (i in 1:6){
  
  temp_freq <- dt_list_f[[i]]
  
  temp_freq[, `:=` (Region = i)]
  
  lower_frequency <- rbind(lower_frequency, temp_freq)
  
}

lower_severity <- data.table()

for (i in 1:6) {
  
  temp_sev <- dt_list_s[[i]]
  
  temp_sev[, `:=` (Region = i)]
  
  lower_severity <- rbind(lower_severity, temp_sev)
  
}


lower_agg <- lower_frequency * lower_severity
lower_agg[, Region := sqrt(Region)]

# Middle

middle_frequency <- data.table()

for (i in 1:6){
  
  temp_freq <- dt_list_f[[i+6]]
  
  temp_freq[, `:=` (Region = i)]
  
  middle_frequency <- rbind(middle_frequency, temp_freq)
  
}

middle_severity <- data.table()

for (i in 1:6) {
  
  temp_sev <- dt_list_s[[i+6]]
  
  temp_sev[, `:=` (Region = i)]
  
  middle_severity <- rbind(middle_severity, temp_sev)
  
}


middle_agg <- middle_frequency * middle_severity
middle_agg[, Region := sqrt(Region)]

# Middle

middle_frequency <- data.table()

for (i in 1:6){
  
  temp_freq <- dt_list_f[[i+6]]
  
  temp_freq[, `:=` (Region = i)]
  
  middle_frequency <- rbind(middle_frequency, temp_freq)
  
}

middle_severity <- data.table()

for (i in 1:6) {
  
  temp_sev <- dt_list_s[[i+6]]
  
  temp_sev[, `:=` (Region = i)]
  
  middle_severity <- rbind(middle_severity, temp_sev)
  
}


middle_agg <- middle_frequency * middle_severity
middle_agg[, Region := sqrt(Region)]

# Upper

upper_frequency <- data.table()

for (i in 1:3){
  
  temp_freq <- dt_list_f[[i+12]]
  
  temp_freq[, `:=` (Region = i)]
  
  upper_frequency <- rbind(upper_frequency, temp_freq)
  
}

upper_severity <- data.table()

for (i in 1:3) {
  
  temp_sev <- dt_list_s[[i+12]]
  
  temp_sev[, `:=` (Region = i)]
  
  upper_severity <- rbind(upper_severity, temp_sev)
  
}


upper_agg <- upper_frequency * upper_severity
upper_agg[, Region := sqrt(Region)]

# Freak

freak_frequency <- dt_list_f[[13]]

freak_frequency[, `:=` (Region = i)]

freak_severity <- dt_list_s[[13]]
  
freak_severity[, `:=` (Region = i)]

freak_agg <- freak_frequency * freak_severity
freak_agg[, Region := sqrt(Region)]

# Calculating random economic metrics
GDP = baseline[Regions == r, GDP2020_1000 := (GDP2020_1000 * g]*g,
Population = baseline[Regions == r, Population]*p,
total = as.numeric(unlist(dtf[, ..sim] * dts[, ..sim])) * 1000 * inflationsim * freqx,
housevalue = baseline[Regions == r, MedianHouse] * inflationsim, 
pphouse = baseline[Regions == r, PpHouse],
injuries = as.numeric(unlist(dtf[, ..sim])) * freqx * injury,
fatalities = as.numeric(unlist(dtf[, ..sim])) * freqx * fatality)
total <- total[, no_houses := total/housevalue][, no_pp := no_houses*pphouse][, GDPpercapita := GDP/Population]


