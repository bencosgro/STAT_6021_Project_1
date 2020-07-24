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

data$colorscore <- revalue(data$cut, c("J"="0", "I"="1", "H"="2", "G"="3", "F"="4", "E"="5", "D"="6"))
data$clarityscore <- revalue(data$clarity, c("SI2"="0", "SI1"="1", "VS2"="2", "VS1"="3","VVS2"="4", "VVS1"="5", "IF"="6", "FL"="7"))

attach(data)

data$colorscore <- as.numeric(colorscore)
data$clarityscore <- as.numeric(clarityscore)
plot(cutscore~clarityscore)

datFormatted <- data[, c(5, 1, 6, 7, 3)]