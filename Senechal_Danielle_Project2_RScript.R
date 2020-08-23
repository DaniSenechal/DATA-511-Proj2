########################################################
# Project 2
# Danielle Senechal
# DATA 511 Intro to Data Science
# June 23rd, 2020
########################################################

library(ggplot2)

#################### Part 1 ####################

# read in dataset
camera <- read.csv("/Users/daniellesenechal/Documents/CCSU/DATA 511/Project 2/Cameras.csv")
# View(camera)

########## Question 1 ##########
# Regress Score on Price
reg1 <- lm(Score ~ Price, data = camera); reg1

# plot showing the data from cameras data set
plot(camera$Price, camera$Score, main = "Score and Camera Price", xlab = "Price", 
     ylab = "Score", pch = 19)
# add line of best fit
abline(reg1, col = "red")


########## Question 2 ##########
# use regression equation generated above to predict score of $70 camera
(0.05525 * 70) + 46.66880

# get r^2 and s
summary(reg1)


########## Question 3 ##########
# standardize reg1 to see which standardized residuals are above 2 
stand.cam <- rstandard(reg1); stand.cam

# get residuals of reg1
residuals(reg1)


########## Question 4 ##########
plot(hatvalues(reg1), type = "h", main = "Leverage Values", ylab = "hat values")

# plot showing the data from cameras data set
plot(camera$Price, camera$Score, main = "Score and Camera Price",
     xlab = "Price", 
     ylab = "Score", pch = 19)
# add line of best fit that excludes camera 17 for comparison
lev.cam <- camera[-c(17), ] # remove camera 17
# create new regression line based off data excluding camera 17
abline(lm(Score ~ Price, data = lev.cam), col = "red")


########## Question 5 ##########
# Identify and interpret any influential observations.  
# find median of the F distribution
qf(0.5, 1, 26) 
# get the 25th percetile of the distribution
qf(0.25, 1, 26) 

# cooks distance values on regession from question 1
cooks.distance(reg1)
# sort cooks distsnce values in increasing order
sort(cooks.distance(reg1))


########## Question 6 ##########
# generate residuals vs fitted and normal Q-Q plot
plot(reg1, pch = 19)


########## Question 7 ##########
# output summary and use the coefficient category to get the needed statistics
summary(reg1)


########## Question 8 ##########
# assign new price for new camera for prediction
newcamera <- data.frame(Price = 250)

# use the new camera to get a 99% prerdiction interval
predict(reg1, newcamera, interval = "prediction", level = 0.99)


#################### Part 2 ####################

# read in dataset
facebook <- read.csv("/Users/daniellesenechal/Documents/CCSU/DATA 511/Project 2/Facebook.csv")
# View(facebook)
summary(facebook)

########## Question 9 ##########
# Get a boxplot of student motivation, by instructor self-disclosure level
boxplot(facebook$S.M ~ facebook$Level, 
        main = "Student Motivation by Instructor Self-Disclosure",
        xlab = "Level", ylab = "Student Motivation")


########## Question 10 ##########
# verify ANOVA assumptions: normal quantile plot of student motivation by level
qplot(sample = S.M, 
      data = facebook, color = Level)

# Bartlett's test for homogeneity of variance
bartlett.test(S.M ~ Level, data = facebook)


########## Question 11 ##########
# performing the ANOVA
ANOVAfit <- (aov(S.M ~ Level, data = facebook))

# results from ANOVA
summary(ANOVAfit)


########## Question 12 ##########
# Performing multiple comparisons
TukeyHSD(ANOVAfit)

