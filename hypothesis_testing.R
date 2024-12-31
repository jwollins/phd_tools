# WORKSHOP 2: Hypothesis testing

# INSTRUCTIONS

# For the first half of this practical we will use the heart rate dataset described last week: 

# An experiment was conducted by students at The Imaginary University to explore the nature 
# of the relationship between a person's heart rate and the frequency at which that person stepped up and down on 
# steps of various heights. Heart rate after exercise was measured in beats per minute. 

# There were two different step heights: 5.75 inches (coded as 0), and 11.5 inches (coded as 1). 
# There were three rates of stepping: 14 steps/min. (coded as 0), 21 steps/min. (coded as 1), and 28 steps/min. (coded as 2). 

# This resulted in six possible height/frequency combinations, each of which was tested 5 times, by different subjects each time,
# giving a total of 30 different subjects. 

# Each subject performed the activity for three minutes. 

# Subjects were kept on pace by the beat of an electric metronome. One experimenter counted the subject's pulse for 20 
# seconds before and after each trial. Another experimenter kept track of the time spent stepping. The same pair of 
# experimenters worked throughout, to minimise variation in the measurements.

# There are 30 datapoints in the dataset, and the variables are:

# Order, the order in which the participants took part in the experiment
# Height, the step height (coded as 0 or 1, see above)
# Frequency, the rate of  stepping (coded as 0,1 or 2 , see above)
# RestHR, resting hearth rate
# HR, heart rate after exercise

# This dataset is a modified version of an original from the Data and Story Library (http://dasl.datadesk.com)



# 1. PRACTISE TASKS

# 1.1 Make sure that your working directory is the folder where you saved the "Stepping.csv" file last week.

# 1.2 Performing a 2 sample T test

# enter the following at the command line:

> Data<-read.table("Stepping.csv", header=TRUE, sep= ",")
> Diffs=Data$HR-Data$RestHR
> t.test(Diffs~Data$Height, var.equal=TRUE)

# What does the output suggest about the effect of the height of the step on the change in heart rate?
# At which step height is there a greater increase in heart rate?

# The above code tells R to perform a 2 sample T test, making the assumption that the two groups being compared share the same variance.
# This is the standard way to perform a 2 sample T test (but NOT the default in R, so you do need to include the var.equal part of the code!)

# See if you can change the code so that the 2 sample T test does not assume equal variances.

# What happens to the results of the test when you do not assume the two groups being compared share the same variance?

# Now try this 

Sample1 <-(Data$HR[which (Data$Height==0)]-Data$RestHR[which (Data$Height==0)])
Sample2 <-(Data$HR[which (Data$Height==1)]-Data$RestHR[which (Data$Height==1)])
t.test(Sample1, Sample2, var.equal=TRUE)

# Hopefully you can now see that there is more than one way to perform the same 2 sample T test in R

# What happens when you use the following code :
t.test(Sample1)

#What type of T test has been performed now ?
#One sample Paired TTest
#Can you work out how to test whether the mean of Sample1 alone is signficantly different from 15?


# 1.3 Performing analysis of variance

# enter the following at the command line:

model1 <- lm(Diffs~as.factor(Data$Height))
anova(model1)

# NOTE- the  "as.factor" part of the code is telling R to ignore the fact that the height codes '0' and '1' have a numerical meaning,
# and instead treat them as two categories (which in this case are low and high steps). In this simple case, it would not have
# affected our conclusions if we had let R assume that we wanted it to interpret '0' and '1' as numbers. However, depending on the nature of the data, 
# the coding you have used in the data and the test you want to perform, this can lead to problems if you are not careful! 

# Compare the results of this ANOVA with the two sample T tests you have just carried out. Do you notice any similarities or differences?

# 1.4 Performing analysis of variance, including an interaction

# enter the following at the command line:

model2 <- lm(Diffs~as.factor(Data$Height)*as.factor(Data$Frequency))
anova(model2)

# based on what we learned in the lecture, what do you conclude from this analsysis?

# 1.5 Testing whether the assumptions of ANOVA are met

# enter the following:

plot(model2)

# press enter to cycle through 4 different plots. The first two plots (Residuals versus fitted and Normal Q-Q) should look familiar based on the lecture. 
# You can ignore the other two.
# Does it appear that the assumptions of ANOVA are met?


# 1.6 Performing a Mann Whitney Wilcoxson test

# It is instructive to compare the non parametric Mann Whitney Wilcoxson test with the T tests you have already performed.
# Enter the following:

wilcox.test(Diffs~Data$Height) 

# how does this test compare with the T tests?
# Note that R has called the test statistic "W", which is similar but not always exactly the same as the "U"
# we discussed in the lecture. For more information on what R is reporting here, please see my extra notes 
# provided in the workshop section on Moodle.


# For the second part of this practial, we shall consider the following two datasets:

#FIRST DATASET

# The rainfall in acre-feet from 52 clouds, 26 of which were selected at random and seeded with silver nitrate.

# Variable Names:
# Unseeded_Clouds: Amount of rainfall from unseeded clouds (in acre-feet)
# Seeded_Clouds: Amount of rainfall from seeded clouds with silver nitrate (in acre-feet)

# Source: Chambers, Cleveland, Kleiner, and Tukey. (1983). Graphical Methods for Data Analysis. Wadsworth International Group, Belmont, CA, 351. Original Source: Simpson, Alsen, and Eden. (1975). A Bayesian analysis of a multiplicative treatment effect in weather modification. Technometrics 17, 161-166. Accessed via the Data and Story Library (http://dasl.datadesk.com)

#the data is available in the file "clouds.csv"

# SECOND DATASET
#A farmer was interested in which of three different varieties of rye (A, B and C) gave the greatest yield when grown on her farm, and whether the 
# field in which the rye was grown made a difference. 5 plots of each of the three varieties were planted in each of three fields (top, middle and bottom).
# The yield from each plot was recorded.

# There are 45 datapoints in the dataset, and the variables are:

# WEIGHT, the final weight yielded
# FIELD (top, mid or bottom)
# VARIETY (A, B or C).

# the data is available in the file "ryedata.csv"


# 2. CHALLENGE TASKS

# 2.1 Read in the clouds dataset described above.

# 2.2 Perform an appropriate analysis to test whether silver nitrate seeding has an effect on rainfall. Don't forget to check whether the assumptions of your analysis have been met!


#Hint - to check whether data are normally distributed, it can help to visualise the data as a histogram, e.g. (where 'y' represents whatever data you are investigating)
hist(y)
#or you can use a normal quantile plot
qqnorm(y)

# 2.3 Read in the rye dataset described above.

# 2.4 Perform an appropriate analysis to test whether there is any effect of rye type or field type on yield of rye, and whether the effect of rye type 
# on yield of rye varies according to the field.

variety_model <- lm(rye_data$WEIGHT ~ as.factor(rye_data$VARIETY))
field_model <- lm(rye_data$WEIGHT ~ as.factor(rye_data$FIELD))

Variety_field_model <- lm(rye_data$WEIGHT ~ as.factor(rye_data$VARIETY) + as.factor(rye_data$FIELD))
  
# 2.5 Check if the assumptions of the test you have just performed are likely to have been met. What do you conclude?  Can you visualise the data and identify any specific issues that might be causing problems? What do you suggest the farmer tries next?
