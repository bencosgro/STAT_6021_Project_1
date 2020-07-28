#setwd("/Users/ben_cosgo/Working_directory/STAT_6021_Project_1")
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

#can we order the x axis by worst class to best class?
#redorder color from worst class to best class
data$color <- factor(data$color, levels=c("J","I","H","G","F","E","D"))
#check reorder
table(data$color)
#reorder clarity from worst to best class
data$clarity <- factor(data$clarity, levels=c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF", "FL"))
#check reorder
table(data$clarity)
#reorder cut from worst to best class
data$cut <- factor(data$cut, levels=c("Good","Very Good","Ideal","Astor Ideal"))
#check redorder
table(data$cut)
#every color class has lots of outliers for high prices - the better the color, the more spread in the outliers
boxplot(price~color,data=data, main="Price vs Color", xlab="Color of Diamond", ylab="Price of Diamond")
#every clarity class has lots of outliers for high prices
boxplot(price~clarity,data=data, main="Price vs Clarity", xlab="Clarity of Diamond", ylab="Price of Diamond")
#every cut class has lots of outliers for high prices - the better the cut, the more spread in the outliers
boxplot(price~cut,data=data, main="Price vs Cut", xlab="Cut of Diamond", ylab="Price of Diamond")

# data$colorscore <- as.numeric(colorscore)
# data$clarityscore <- as.numeric(clarityscore)
# plot(colorscore~clarityscore)

head(data)
#datFormatted <- data[, c(5, 1, 6, 7, 8)] # new data set that only has what we will use 
#head(datFormatted)
#attach(datFormatted)

# lets get a scatter plot first 

title = "Price VS. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)


# perform the linear regression to our newly formatted dataset 
lmodel <- lm(data)
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

data_fixed <- data
data_fixed$price <- newPrice

attach(data_fixed)

lmodel_fixed <- lm(data_fixed)
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

datFormatted_fixed2 <- data_fixed
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


