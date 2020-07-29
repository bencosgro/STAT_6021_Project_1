setwd("/Users/ben_cosgo/Working_directory/STAT_6021_Project_1")
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
#reorder color from worst class to best class
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
levene.test(price,color)
boxplot(price~color,data=data, main="Price vs Color", xlab="Color of Diamond", ylab="Price of Diamond")
#every clarity class has lots of outliers for high prices
levene.test(price,clarity)
boxplot(price~clarity,data=data, main="Price vs Clarity", xlab="Clarity of Diamond", ylab="Price of Diamond")
#every cut class has lots of outliers for high prices - the better the cut, the more spread in the outliers
levene.test(price,cut)
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
lmodel <- lm(price~.,data)
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

boxcox(lmodel, lambda=seq(.37,0.375,by=0.001))

lamb <- 0.372

newPrice <- price^lamb

data_fixed <- data
data_fixed$price <- newPrice

attach(data_fixed)

lmodel_fixed <- lm(price~.,data_fixed)
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

lamb2 <- .69

newCarat <- carat^lamb2

datFormatted_fixed2 <- data_fixed
datFormatted_fixed2$carat <- newCarat
attach(datFormatted_fixed2)

lmodel_fixed2 <- lm(price~.,datFormatted_fixed2)
summary(lmodel_fixed2)

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

### we have transformed both the response and numerical predictor and the model is more compliant with the linear model assumtions
### we will now see if there are multicollinearity 


# perform vif analysis 
vif(lmodel_fixed2)
# none of the scores are over 10 ( all of them are actually less than 4)

# perform anova test 
anova(lmodel_fixed2)
# all of the predictors are significant 
# current data : datFormatted_fixed2

# perform pairwise test to see if all categorical variables are significantly different in their means for price for each value
pairwise_clarity<-glht(lmodel_fixed2, linfct = mcp(clarity= "Tukey"))
summary(pairwise_clarity)
pairwise_color<-glht(lmodel_fixed2, linfct = mcp(color= "Tukey"))
summary(pairwise_color)
pairwise_cut<-glht(lmodel_fixed2, linfct = mcp(cut= "Tukey"))
summary(pairwise_cut)
# they are all significantly different for all pairs

# let s do partial F test for each categorical variable
# first make the reduced linear regression modellmodel_noClarity <- lm(price~carat+color+cut)
lmodel_noClarity <- lm(price~carat+color+cut)
lmodel_noColor <- lm(price~carat+clarity+cut)
lmodel_noCut <- lm(price~carat+clarity+color)

# perform the partial F test
# for clarity
anova(lmodel_noClarity, lmodel_fixed2)
# for color
anova(lmodel_noColor, lmodel_fixed2)
# for cut
anova(lmodel_noCut, lmodel_fixed2)

# partial F tests were significant for all of the categorical variables
# we can not drop any of the categorical variables. 

