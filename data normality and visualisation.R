# data visualisation and Normality Test in R
# J Collins 

## Setup ####

library(dplyr)
library(ggpubr)

### set working directory ####
setwd(dir = "/Users/u1764794/OneDrive - Harper Adams University/Shavington & Cloverley/Farm Trials/Data Analysis/HH yield/")

### Import your data into R ####
my_data <- read.csv(file = "~/OneDrive - Harper Adams University/Shavington & Cloverley/Farm Trials/Data Collection/Biomass/ALL_FIELDS_ALL_DATA_2022.csv", header = TRUE)


## Visual tests ####

### Desity plot ####

# Density plot and Q-Q plot can be used to check normality visually.
# 
# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.

ggdensity(my_data$bean_t_ha, 
          main = "Density plot of yield data t/ha",
          xlab = "Yield t/ha")


### Q plot ####

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a 
# given sample and the normal distribution. A 45-degree reference line is also plotted.

ggqqplot(my_data$bean_t_ha)




## Normality test ####

# Visual inspection, described in the previous section, is usually unreliable. 
# It’s possible to use a significance test comparing the sample distribution to a 
# normal one in order to ascertain whether data show or not a serious deviation from normality.
# 
# There are several methods for normality test such as Kolmogorov-Smirnov (K-S)
# normality test and Shapiro-Wilk’s test.

# Shapiro-Wilk’s method is widely recommended for normality test and it provides 
# better power than K-S. It is based on the correlation between the data and the corresponding normal scores.


shapiro.test(my_data$bean_t_ha)

# From the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. In other words, we can assume the normality.





