#setwd("/Users/ben_cosgo/Working_directory") change
#setwd("/c/Users/joony/Documents/myGit/STAT_6021_Project_1")

library("tidyverse")
library(plyr)
library(dplyr)

data <- read.csv("clean_diamond_data.csv")
attach(data)

table(data$color)
table(data$clarity)
table(data$cut)

data$cutscore <- revalue(data$cut, c("Good"="0", "Very Good"="1", "Ideal"="2", "Astor Ideal"="3"))
data$clarityscore <- revalue(data$clarity, c("SI2"="0", "SI1"="1", "VS2"="2", "VS1"="3","VVS2"="4", "VVS1"="5", "IF"="6", "FL"="7"))

attach(data)

data$cutscore <- as.numeric(cutscore)
data$clarityscore <- as.numeric(clarityscore)
plot(cutscore~clarityscore)

