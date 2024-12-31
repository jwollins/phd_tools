## HEADER ####
## Who: Joe Collins / Ed Harris 
## what: Power Analysis / Power Curve Soil Organic Carbon - Reduced Tillage Systems 
## Last edited: 2022-08-01

# Data from: 

# Bulk Density 
#Yuan, Li & Li, Zhou & Chang, Scott & Cui, Song & Jagadamma, Sindhu & Qingping, Zhang & Cai, Yanjiang. (2020). 
#Residue retention promotes soil carbon accumulation in minimum tillage systems: Implications for conservation 
#agriculture. Science of The Total Environment. 740. 140147. 10.1016/j.scitotenv.2020.140147. 

## CONTENTS ####

## 00 Working Directory ####

getwd() # get the current working directory
setwd("/Users/u1764794/OneDrive - Harper Adams University/Resources/Power_Analysis /")  # Set the working directory

## 01 Packages ####	

# Required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(pwr)
cohen.ES  # Give the conventional effect size (small, medium, large) for the tests available in this package



## 02 Crop Yield ####

### 02.1 Simulated Power Analysis ####	

simn <- c(600, 800, 1000, 1200, 1400, 1600)  # This is the Simulated number of observations (n) for the power analysis 
simcd <- c(0.02, 0.04, 0.06, 0.08, 0.1, 0.12)  # This is the simulated Effect Size (d)

mymat <- matrix(nrow=length(simn), ncol = length(simcd))  # Create the empty matrix the required size 
row.names(mymat) <- simn                                 # Add the row names 
colnames(mymat) <- simcd                                # Add col names 

for(i in 1:length(simn)){                           # for each position in matrix row
  for(j in 1:length(simcd)){                        # for each position in matrix column 
    mymat[i,j] <- pwr.t.test(n = simn[i],           # run pwr test using i and j positions from vectors 
                             d = simcd[j], 
                             sig.level = 0.05,      # generate power
                             power = NULL)$power    # add to matrix 
  }
}

print(mymat)     # prints the matrix to the screen 



### 02.2 Power Curve Plot ####	

setwd("~/OneDrive - Harper Adams University/Data/agronomy/plots/")

# Convert the matrix into a tidy data frame
power_data <- as.data.frame(mymat) %>%
  mutate(Effect_Size = simcd) %>%                  # Add effect size as a column
  pivot_longer(cols = -Effect_Size,               # Reshape the data: one column per sample size
               names_to = "Sample_Size_Index", 
               values_to = "Power") %>%
  mutate(Sample_Size = rep(simn, times = nrow(mymat))) # Map the sample size values correctly

# Plot the power curve
ggplot(power_data, aes(x = Sample_Size, y = Power, color = factor(Effect_Size), group = Effect_Size)) +
  geom_line(size = 1) +                           # Add lines for each effect size
  geom_point(size = 2) +                          # Add points for visibility
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.7) +  # Power threshold at 0.8
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "blue", size = 0.7) + # Power threshold at 0.95
  labs(
    x = substitute(paste("Number of Yield Observations ", italic('(n)'))),
    y = expression(Simulated~Power~(alpha)),
    color = "Effect Size (Cohen's d)"
  ) +
  scale_x_continuous(breaks = simn) +             # Set custom x-axis breaks for sample sizes
  scale_y_continuous(limits = c(0, 1)) +          # Limit y-axis to power values [0, 1]
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

ggsave(filename = "yield_power_analysis.png", width = 10, height = 4)





## 03 Bulk Density ####

### 03.1 Simulated Power Analysis ####	

# Simulated number of observations
simn <- seq(from = 40, to = 400, by = 40)

# Simulated effect sizes
simcd <- seq(from = 0.1, to = 1, length.out = 10)

simcd <- round(x = simcd, digits = 3)

# Create the matrix with row and column names
mymat <- matrix(nrow = length(simn), ncol = length(simcd))
row.names(mymat) <- simn
colnames(mymat) <- simcd

# Fill the matrix with power calculations
for (i in 1:length(simn)) {
  for (j in 1:length(simcd)) {
    mymat[i, j] <- pwr.t.test(
      n = simn[i],
      d = simcd[j],
      sig.level = 0.05,
      power = NULL
    )$power
  }
}

# Check the result
mymat





### 03.2 Power Curve Plot ####	

# Convert the matrix into a tidy data frame
power_data <- as.data.frame(mymat) %>%
  mutate(Effect_Size = simcd) %>%                  # Add effect size as a column
  pivot_longer(cols = -Effect_Size,               # Reshape the data: one column per sample size
               names_to = "Sample_Size_Index", 
               values_to = "Power") %>%
  mutate(Sample_Size = rep(simn, times = nrow(mymat))) # Map the sample size values correctly

# Plot the power curve
ggplot(power_data, aes(x = Sample_Size, y = Power, color = factor(Effect_Size), group = Effect_Size)) +
  geom_line(size = 1) +                           # Add lines for each effect size
  geom_point(size = 2) +                          # Add points for visibility
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.7) +  # Power threshold at 0.8
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "blue", size = 0.7) + # Power threshold at 0.95
  labs(
    x = substitute(paste("Number of Bulk Density Observations ", italic('(n)'))),
    y = expression(Simulated~Power~(alpha)),
    color = "Effect Size (Cohen's d)"
  ) +
  scale_x_continuous(breaks = simn) +             # Set custom x-axis breaks for sample sizes
  scale_y_continuous(limits = c(0, 1)) +          # Limit y-axis to power values [0, 1]
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

ggsave(filename = "~/OneDrive - Harper Adams University/Data/Soil/bulk_density/plots/bd_power_analysis.png", 
       width = 10, height = 4)





## 04 SOC ####

### Simulated Power Analysis ####	

# Simulated number of observations
simn <- seq(from = 50, to = 500, by = 50)

# Simulated effect sizes
simcd <- seq(from = 0.2, to = 1.2, length.out = 10)

simcd <- round(x = simcd, digits = 2)

# Create the matrix with row and column names
mymat <- matrix(nrow = length(simn), ncol = length(simcd))
row.names(mymat) <- simn
colnames(mymat) <- simcd

# Fill the matrix with power calculations
for (i in 1:length(simn)) {
  for (j in 1:length(simcd)) {
    mymat[i, j] <- pwr.t.test(
      n = simn[i],
      d = simcd[j],
      sig.level = 0.05,
      power = NULL
    )$power
  }
}

# Check the result
mymat





### 03.2 Power Curve Plot ####	

# Convert the matrix into a tidy data frame
power_data <- as.data.frame(mymat) %>%
  mutate(Effect_Size = simcd) %>%                  # Add effect size as a column
  pivot_longer(cols = -Effect_Size,               # Reshape the data: one column per sample size
               names_to = "Sample_Size_Index", 
               values_to = "Power") %>%
  mutate(Sample_Size = rep(simn, times = nrow(mymat))) # Map the sample size values correctly

# Plot the power curve
ggplot(power_data, aes(x = Sample_Size, y = Power, color = factor(Effect_Size), group = Effect_Size)) +
  geom_line(size = 1) +                           # Add lines for each effect size
  geom_point(size = 2) +                          # Add points for visibility
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.7) +  # Power threshold at 0.8
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "blue", size = 0.7) + # Power threshold at 0.95
  labs(
    x = substitute(paste("Number of SOC Observations ", italic('(n)'))),
    y = expression(Simulated~Power~(alpha)),
    color = "Effect Size (Cohen's d)"
  ) +
  scale_x_continuous(breaks = simn) +             # Set custom x-axis breaks for sample sizes
  scale_y_continuous(limits = c(0, 1)) +          # Limit y-axis to power values [0, 1]
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

ggsave(filename = "~/OneDrive - Harper Adams University/Data/Soil/soil_chemistry/plots/soc_power_analysis.png", 
       width = 10, height = 4)




