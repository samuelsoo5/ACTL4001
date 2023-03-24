library(data.table)
library(ggplot2)
library(fitdistrplus)
library(actuar)

hazard_list <- unique(hd[, HazardEventGrouped])

#hazard <- hazard_list[8] # hurricane

hazard <- hazard_list[12] # wildfire


hazard_data <- hd[HazardEventGrouped == hazard
                  & PropertyDamage > 0]

hazard_pd <- as.numeric(lower_storm_1[, PropertyDamage])

ggplot() + geom_point(data = hazard_data, aes(x = factor(Year), y = as.numeric(PropertyDamage))) + ggtitle(hazard)

ggplot() + geom_histogram(data = hazard_data, aes(x = as.numeric(PropertyDamage))) + ggtitle(hazard)


# Analysing heavy tailed-ness of data

set.seed(1)          # for reproducibility 
Z <- rexp(1000)      # random sample from exponential distribution
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(as.numeric(hazard_data[, PropertyDamage]),p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="blue", lty=2)

# Gamma

hazard.gam <- fitdist(data = as.numeric(hazard_data[, PropertyDamage])/1000000, 
                      distr = "gamma", method = "mle")

plot(hazard.gam)

gofstat(hazard.gam)

# Weibull

hazard.wb <- fitdist(data = as.numeric(hazard_data[, PropertyDamage])/1000000, 
                     distr = "weibull", method = "mle")

plot(hazard.wb)

gofstat(hazard.wb)

# Pareto

hazard.pr <- fitdist(data = as.numeric(hazard_data[, PropertyDamage])/10000, 
                     "pareto", start = list(shape=1, scale=0), method = "mle")

plot(hazard.pr)

gofstat(hazard.pr)

# Lognormal

hazard.ln <- fitdist(data = as.numeric(hazard_data[, PropertyDamage])/100000, 
                     "lnorm", method = "mle")

plot(hazard.ln)

gofstat(hazard.ln)


