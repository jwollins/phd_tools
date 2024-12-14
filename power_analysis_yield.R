## HEADER ####
## Who: Joe Collins / Ed Harris 
## what: Power Analysis / Power Curve Soil Organic Carbon - Reduced Tillage Systems 
## Last edited: 2022-08-01

# Data from: 
#Yuan, Li & Li, Zhou & Chang, Scott & Cui, Song & Jagadamma, Sindhu & Qingping, Zhang & Cai, Yanjiang. (2020). 
#Residue retention promotes soil carbon accumulation in minimum tillage systems: Implications for conservation 
#agriculture. Science of The Total Environment. 740. 140147. 10.1016/j.scitotenv.2020.140147. 

## CONTENTS ####

## 00 Working Directory ####

getwd() # get the current working directory
setwd("/Users/u1764794/OneDrive - Harper Adams University/Resources/Power_Analysis /")  # Set the working directory

## 01 Packages ####	

library(pwr)
cohen.ES  # Give the conventional effect size (small, medium, large) for the tests available in this package

## 02 Simulated Power Analysis ####	

simn <- c(500, 600, 700, 800, 900, 1000)  # This is the Simulated number of observations (n) for the power analysis 
simcd <- c(0.04, 0.06, 0.08, 0.1, 0.12, 0.14)  # This is the simulated Effect Size (d)

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

## 04 Power Curve Plot ####	

setwd("~/OneDrive - Harper Adams University/Data/agronomy/plots/")
png(filename="yield_power_analysis.png")

par(mar=c(5, 4, 4, 8), xpd=TRUE)

matplot(x=mymat, type='b', 
        lty = c(1,2), 
        xlab = "Simulated n - Number of Yield Observations", ylab = "Simulated Power",   # Axis Labels 
        pch = 1:8,         # Symbols
        xaxt="n",          # remove x axis 
        col = "black")     # Line colour 

# abline(h=0.8)                      #AB line 
# abline(h=0.95)                      #AB line 


legend(x = "right",    # Position
       inset = c(- 0.3, 0),    
       legend = simcd,             # Legend texts
       title = "Effect Size (d)",
       lty = c(1, 2),              # Line types
       col = "black",              # Line colors
       pch = 1:8,                  # Symbols 
       lwd = 1,)                   # Line width
axis(side = 1, at = 1:length(simn), labels = simn)  # Add custom axis back on to plot with vector labels

dev.off()






png(filename = "yield_power_analysis.png", 
    width = 2000,        # Width in pixels
    height = 1500,       # Height in pixels
    res = 300)           # Resolution in DPI

par(mar = c(5, 4, 4, 8), xpd = TRUE)

matplot(x = mymat, type = 'b', 
        lty = c(1, 2), 
        xlab = "Simulated n - Number of Yield Observations", 
        ylab = "Simulated Power",   # Axis Labels 
        pch = 1:8,                  # Symbols
        xaxt = "n",                 # Remove x-axis 
        col = "black")              # Line color

# Add legend
legend(x = "right",                # Position
       inset = c(-0.3, 0),         
       legend = simcd,             # Legend texts
       title = "Effect Size (d)",
       lty = c(1, 2),              # Line types
       col = "black",              # Line colors
       pch = 1:8,                  # Symbols 
       lwd = 1)                    # Line width

# Add custom x-axis
axis(side = 1, at = 1:length(simn), labels = simn)

dev.off()


