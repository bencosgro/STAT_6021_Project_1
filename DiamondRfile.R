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
boxplot(price~color,data=data, main="Price vs Color", xlab="Color", ylab="Price")
#every clarity class has lots of outliers for high prices
levene.test(price,clarity)
boxplot(price~clarity,data=data, main="Price vs Clarity", xlab="Clarity", ylab="Price ")
#every cut class has lots of outliers for high prices - the better the cut, the more spread in the outliers
levene.test(price,cut)
boxplot(price~cut,data=data, main="Price vs Cut", xlab="Cut", ylab="Price")

# data$colorscore <- as.numeric(colorscore)
# data$clarityscore <- as.numeric(clarityscore)
# plot(colorscore~clarityscore)

head(data)
#datFormatted <- data[, c(5, 1, 6, 7, 8)] # new data set that only has what we will use 
#head(datFormatted)
#attach(datFormatted)

# lets get a scatter plot first 

title = "Price vs. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)

#let's plot carat vs price by colorscore
a1 <- subset(data, color=="J")
a2 <- subset(data, color=="I")
a3 <- subset(data, color=="H")
a4 <- subset(data, color=="G")
a5 <- subset(data, color=="F")
a6 <- subset(data, color=="E")
a7 <- subset(data, color=="D")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
reg5 <- lm(price~carat, data=a5)
reg6 <- lm(price~carat, data=a6)
reg7 <- lm(price~carat, data=a7)
plot(carat, price, main="Price vs. Carat by Color", xlab=xlab, ylab=ylab)
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
points(a5$carat, a5$price, col='pink')
points(a6$carat, a6$price, col='purple')
points(a7$carat, a7$price, col='orange')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
abline(reg5, lty=5, col='pink')
abline(reg6, lty=6, col='purple')
abline(reg7, lty=7, col='orange')
legend("topright", c("J","I","H","G","F","E","D"), lty=c(1,2,3,4,5,6,7), col=c('black','red','blue','green','pink','purple','orange'))
#the slopes for all of the lines are similar, but there still may be interaction

#let's plot carat vs price by clarityscore
a1 <- subset(data, clarity=="SI2")
a2 <- subset(data, clarity=="SI1")
a3 <- subset(data, clarity=="VS2")
a4 <- subset(data, clarity=="VS1")
a5 <- subset(data, clarity=="VVS2")
a6 <- subset(data, clarity=="VVS1")
a7 <- subset(data, clarity=="IF")
a8 <- subset(data, clarity=="FL")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
reg5 <- lm(price~carat, data=a5)
reg6 <- lm(price~carat, data=a6)
reg7 <- lm(price~carat, data=a7)
reg8 <- lm(price~carat, data=a8)
plot(carat, price, main="Price vs. Carat by Clarity", xlab=xlab, ylab=ylab)
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
points(a5$carat, a5$price, col='pink')
points(a6$carat, a6$price, col='purple')
points(a7$carat, a7$price, col='orange')
points(a8$carat, a8$price, col='yellow')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
abline(reg5, lty=5, col='pink')
abline(reg6, lty=6, col='purple')
abline(reg7, lty=7, col='orange')
abline(reg8, lty=8, col='yellow')
legend("topright", c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF", "FL"), lty=c(1,2,3,4,5,6,7,8), col=c('black','red','blue','green','pink','purple','orange','yellow'))
#the slopes for all of the lines are similar, but there still may be interaction

#let's plot carat vs price by cutscore
a1 <- subset(data, cut=="Good")
a2 <- subset(data, cut=="Very Good")
a3 <- subset(data, cut=="Ideal")
a4 <- subset(data, cut=="Astor Ideal")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
plot(carat, price, main="Price vs. Carat by Cut", xlab=xlab, ylab=ylab)
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
legend("topright", c("Good","Very Good","Ideal","Astor Ideal"), lty=c(1,2,3,4), col=c('black','red','blue','green'))
#the slopes for all of the lines are similar, but there still may be interaction

# perform the linear regression to our newly formatted dataset 
lmodel <- lm(price~.,data)
summary(lmodel)

abline(lmodel, col="red")
# looks like all of them have significance 

# now we should check if they assumptions are met
##residual plot
plot(lmodel$fitted.values,lmodel$residuals,main="Residual Plot",xlab="Fitted Values", ylab="Residual Values")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel$residuals, main="Autocorrelation Plot")

##QQ plot of residuals
qqnorm(lmodel$residuals, main="Normal Probability Plot")
qqline(lmodel$residuals, col="red")

boxcox(lmodel, lambda=seq(.37,0.375,by=0.001),main="Box-Cox Plot")

lamb <- 0.372

newPrice <- price^lamb

data_fixed <- data
data_fixed$price <- newPrice

attach(data_fixed)

lmodel_fixed <- lm(price~.,data_fixed)
summary(lmodel_fixed)


title = "Price vs. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)

abline(lmodel_fixed, col="red")

plot(lmodel_fixed$fitted.values,lmodel_fixed$residuals,main="Residual Plot",xlab="Fitted Values",ylab="Residual Values")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel_fixed$residuals, main="Autocorrelation Plot")

##QQ plot of residuals
qqnorm(lmodel_fixed$residuals, main="Normal Probability Plot")
qqline(lmodel_fixed$residuals, col="red")

lamb2 <- 0.69

newCarat <- carat^lamb2

datFormatted_fixed2 <- data_fixed
datFormatted_fixed2$carat <- newCarat
attach(datFormatted_fixed2)

#let's plot carat vs price by colorscore
a1 <- subset(datFormatted_fixed2, color=="J")
a2 <- subset(datFormatted_fixed2, color=="I")
a3 <- subset(datFormatted_fixed2, color=="H")
a4 <- subset(datFormatted_fixed2, color=="G")
a5 <- subset(datFormatted_fixed2, color=="F")
a6 <- subset(datFormatted_fixed2, color=="E")
a7 <- subset(datFormatted_fixed2, color=="D")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
reg5 <- lm(price~carat, data=a5)
reg6 <- lm(price~carat, data=a6)
reg7 <- lm(price~carat, data=a7)
plot(carat, price, main="Price vs. Carat by Color", xlab="Carat", ylab="Price")
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
points(a5$carat, a5$price, col='pink')
points(a6$carat, a6$price, col='purple')
points(a7$carat, a7$price, col='orange')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
abline(reg5, lty=5, col='pink')
abline(reg6, lty=6, col='purple')
abline(reg7, lty=7, col='orange')
legend("topright", c("J","I","H","G","F","E","D"), lty=c(1,2,3,4,5,6,7), col=c('black','red','blue','green','pink','purple','orange'))
#the slopes for all of the lines are similar, but there still may be interaction

#let's plot carat vs price by clarityscore
a1 <- subset(datFormatted_fixed2, clarity=="SI2")
a2 <- subset(datFormatted_fixed2, clarity=="SI1")
a3 <- subset(datFormatted_fixed2, clarity=="VS2")
a4 <- subset(datFormatted_fixed2, clarity=="VS1")
a5 <- subset(datFormatted_fixed2, clarity=="VVS2")
a6 <- subset(datFormatted_fixed2, clarity=="VVS1")
a7 <- subset(datFormatted_fixed2, clarity=="IF")
a8 <- subset(datFormatted_fixed2, clarity=="FL")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
reg5 <- lm(price~carat, data=a5)
reg6 <- lm(price~carat, data=a6)
reg7 <- lm(price~carat, data=a7)
reg8 <- lm(price~carat, data=a8)
plot(carat, price, main="Price vs. Carat by Clarity", xlab="Carat", ylab="Price")
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
points(a5$carat, a5$price, col='pink')
points(a6$carat, a6$price, col='purple')
points(a7$carat, a7$price, col='orange')
points(a8$carat, a8$price, col='yellow')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
abline(reg5, lty=5, col='pink')
abline(reg6, lty=6, col='purple')
abline(reg7, lty=7, col='orange')
abline(reg8, lty=8, col='yellow')
legend("topright", c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF", "FL"), lty=c(1,2,3,4,5,6,7,8), col=c('black','red','blue','green','pink','purple','orange','yellow'))
#the slopes for all of the lines are similar, but there still may be interaction

#let's plot carat vs price by cutscore
a1 <- subset(datFormatted_fixed2, cut=="Good")
a2 <- subset(datFormatted_fixed2, cut=="Very Good")
a3 <- subset(datFormatted_fixed2, cut=="Ideal")
a4 <- subset(datFormatted_fixed2, cut=="Astor Ideal")
reg1 <- lm(price~carat, data=a1)
reg2 <- lm(price~carat, data=a2)
reg3 <- lm(price~carat, data=a3)
reg4 <- lm(price~carat, data=a4)
plot(carat, price, main="Price vs. Carat by Cut", xlab="Carat", ylab="Price")
points(a2$carat, a2$price, col='red')
points(a3$carat, a3$price, col='blue')
points(a4$carat, a4$price, col='green')
abline(reg1, lty=1)
abline(reg2, lty=2, col='red')
abline(reg3, lty=3, col='blue')
abline(reg4, lty=4, col='green')
legend("topright", c("Good","Very Good","Ideal","Astor Ideal"), lty=c(1,2,3,4), col=c('black','red','blue','green'))
#the slopes for all of the lines are similar, but there still may be interaction

lmodel_fixed2 <- lm(price~.,datFormatted_fixed2)
summary(lmodel_fixed2)

plot(lmodel_fixed2$fitted.values,lmodel_fixed2$residuals,main="Residual Plot",xlab="Fitted Values",ylab="Residual Values")
abline(h=0,col="red")

title = "Price vs. Carat"
xlab = "Carat"
ylab ="Price"
x = carat
y = price

# Produce a plot. 
plot(x, y, main=title, xlab=xlab, ylab=ylab)

abline(lmodel_fixed2, col="red")

# now we should check if they assumptions are met
##residual plot

plot(lmodel_fixed2$fitted.values,lmodel_fixed2$residuals,main="Residual Plot",xlab="Fitted Values", ylab="Residual Values")
abline(h=0,col="red")

##acf plot of residuals
acf(lmodel_fixed2$residuals,main="Autocorrelation Plot")

##QQ plot of residuals
qqnorm(lmodel_fixed2$residuals, main="Normal Probability Plot")
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

# let's do partial F test for each categorical variable
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

#partial F tests on one interaction
lmodel_interactioncocl <- lm(price~carat+cut+clarity*color)
anova(lmodel_fixed2, lmodel_interactioncocl)
summary(lmodel_interactioncocl)
lmodel_interactioncocu <- lm(price~carat+clarity+cut*color)
anova(lmodel_fixed2, lmodel_interactioncocu)
summary(lmodel_interactioncocu)
lmodel_interactioncaco <- lm(price~clarity+cut+carat*color)
anova(lmodel_fixed2, lmodel_interactioncaco)
summary(lmodel_interactioncaco)
lmodel_interactionclcu <- lm(price~carat+color+clarity*cut)
anova(lmodel_fixed2, lmodel_interactionclcu)
summary(lmodel_interactionclcu)
lmodel_interactionclca <- lm(price~cut+color+clarity*carat)
anova(lmodel_fixed2, lmodel_interactionclca)
summary(lmodel_interactionclca)
lmodel_interactioncacu <- lm(price~clarity+color+carat*cut)
anova(lmodel_fixed2, lmodel_interactioncacu)
summary(lmodel_interactioncacu)
#all significant

#partial f test on all interactions then between all and one
lmodel_fullInteraction <- lm(price~carat*clarity*cut*color)
summary(lmodel_fullInteraction)
anova(lmodel_fixed2, lmodel_fullInteraction)
anova(lmodel_fixed2, lmodel_interactioncaco)
anova(lmodel_fixed2, lmodel_interactioncacu)
anova(lmodel_fixed2, lmodel_interactionclca)
anova(lmodel_fixed2, lmodel_interactionclcu)
anova(lmodel_fixed2, lmodel_interactioncocl)
anova(lmodel_fixed2, lmodel_interactioncocu)
#all significant again
