setwd('~/R practice')
install.packages('janitor')
library(janitor)
library(ggplot2)
library(lubridate)

#Downloading the dataset
UFO <- read.csv("~/Downloads/UFO.csv")

#Exploring the dataset
summary(UFO)

#Cleaning the dataset
table(is.na(UFO))
table(UFO=='')
UFO[UFO==""] <- NA
UFO <- na.omit(UFO)

UFO$datetime <- as.character(UFO$datetime)
UFO$datetime <- dmy_hm(UFO$datetime)
str(UFO$datetime)
UFO <- na.omit(UFO)

UFO$months <- month(UFO$datetime)
UFO$months <- as.character(UFO$months)

UFO$seasons[UFO$months == c(12, 11, 1)] <- 'Winter'
UFO$seasons[UFO$months == c(8, 9, 10)] <- 'Autumn'
UFO$seasons[UFO$months == c(5, 6, 7)] <- 'Summer'
UFO$seasons[UFO$months == c(2, 3, 4)] <- 'Spring'
UFO$seasons[1] <- 'Autumn'
UFO$seasons[nrow(UFO)] <- 'Autumn'
UFO$seasons <- na.locf(UFO$seasons)

table(UFO$seasons)
sort(table(UFO$state), decreasing = TRUE)
head(sort(table(UFO$city), decreasing = TRUE))
head(sort(table(UFO$datetime), decreasing = TRUE))
head(sort(table(UFO$date.posted), decreasing = TRUE))

UFO$duration..seconds. <- as.numeric(UFO$duration..seconds.)
summary(UFO$duration..seconds.)
summary(UFO$duration..hours.min.)
