#setwd("/Users/ben_cosgo/Working_directory") change
#setwd("C:/Users/joony/Documents/myGit/STAT_6021_Project_1")

library("tidyverse")
library(plyr)
library(dplyr)
library(faraway) 
library(multcomp)
library(lawstat)
library(MASS)

data <- read.csv("clean_diamond_data.csv")
attach(data)

table(data$color)
table(data$clarity)
table(data$cut)

# convert color and clarity to factors that are sorted in increasing order
data$colorscore <- factor(revalue(data$color, c("J"="0", "I"="1", "H"="2", "G"="3", "F"="4", "E"="5", "D"="6")))
data$clarityscore <- factor(revalue(data$clarity, c("SI2"="0", "SI1"="1", "VS2"="2", "VS1"="3","VVS2"="4", "VVS1"="5", "IF"="6", "FL"="7")))
data$cutscore <- factor(revalue(data$cut, c("Good"="0", "Very Good"="1", "Astor Ideal"="2", "Ideal"="3")))

attach(data)

# data$colorscore <- as.numeric(colorscore)
# data$clarityscore <- as.numeric(clarityscore)
# plot(colorscore~clarityscore)

datFormatted <- data[, c(5, 1, 6, 7, 8)] # new data set that only has what we will use 
head(datFormatted)
attach(datFormatted)

# lets get a scatter plot first 

title = "Price VS. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)


# perform the linear regression to our newly formatted dataset 
lmodel <- lm(datFormatted)
summary(lmodel)

abline(lmodel, col="red")
# looks like all of them have significance 

# now we should check if they assumptions are met
##residual plot
plot(lmodel$fitted.values,lmodel$residuals,main="Residual plot")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel$residuals)

##QQ plot of residuals
qqnorm(lmodel$residuals)
qqline(lmodel$residuals, col="red")

boxcox(lmodel, lambda=seq(0.37,0.375,by=0.001))

lamb <- 0.372

newPrice <- price^lamb

datFormatted_fixed <- datFormatted
datFormatted_fixed$price <- newPrice

attach(datFormatted_fixed)

lmodel_fixed <- lm(datFormatted_fixed)
summary(lmodel_fixed)


title = "Price VS. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)

abline(lmodel_fixed, col="red")

plot(lmodel_fixed$fitted.values,lmodel_fixed$residuals,main="Residual plot")
abline(h=0,col="red")


lamb2 <- 1/2

newCarat <- carat^lamb2

datFormatted_fixed2 <- datFormatted_fixed
datFormatted_fixed2$carat <- newCarat
attach(datFormatted_fixed2)

lmodel_fixed2 <- lm(datFormatted_fixed2)

title = "Price VS. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)

abline(lmodel_fixed2, col="red")

# now we should check if they assumptions are met
##residual plot

plot(lmodel_fixed2$fitted.values,lmodel_fixed2$residuals,main="Residual plot")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel_fixed2$residuals)

##QQ plot of residuals
qqnorm(lmodel_fixed2$residuals)
qqline(lmodel_fixed2$residuals, col="red")


# perform vif analysis 
vif(lmodel_fixed2)
# none of the scores are over 10 ( all of them are actually less than 4)

# perform anova test 
anova(lmodel_fixed2)
# all of the predictors are significant 

# even though all of them are significant predictors and we did not see clear multicorrelations, We still believe clarity and carrots are multicorrelated with price

# lets put clarity as interaction. We will see if clarity has significant interaction with carat
lmodel_clarityscore <- lm(price~carat*clarityscore + cutscore + colorscore)
summary(lmodel_clarityscore)


# now we should check if they assumptions are met
##residual plot
plot(lmodel_clarityscore$fitted.values,lmodel_clarityscore$residuals,main="Residual plot")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel_clarityscore$residuals)

##QQ plot of residuals
qqnorm(lmodel_clarityscore$residuals)
qqline(lmodel_clarityscore$residuals, col="red")

# did not meet the assumtions at all


# perform anova test to see if making clarity score is significant
anova(lmodel, lmodel_clarityscore)
# there is a significant interaction between carat and clairty when predicting price



# test of equality of variance across different clarities
levene.test(price,clarityscore)
# the variance is significantly different

# lets just perform tukey's multiple comparison to see
# if average prices are different for different clarities given same carrot
pairwise<-glht(lmodel_clarityscore, linfct = mcp(clarityscore= "Tukey"))
summary(pairwise)
# yes 
