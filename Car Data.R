#Creating a linear model

library(tidyverse)
head(cars)
cars <- data.frame(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main= 'linear relationship')


par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main='speed')
boxplot(cars$dist, main='dist')

cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

linearMod <- lm(data=cars, formula = dist ~ speed)  # build linear regression model on full data
print(linearMod)
linearMod

summary(linearMod)

