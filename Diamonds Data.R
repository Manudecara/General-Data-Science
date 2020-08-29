setwd("~/R practice")
library(tidyverse)

#R practice with the 'diamonds' dataset. ggplot has 'diamonds' data set installed.
#So no need to load it. Just name it

View(diamonds)

#make it appear on the global environment

diamonds <- diamonds

#plot the variable 'cuts'

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill= cut))
           
#now within the plotted variable 'cuts', introduce the variable 'clarity'. 
#The function 'fill' will fill the bars with this new variable.
           
ggplot(data = diamonds, mapping = aes(x = cut, fill= clarity)) + geom_bar()

#Adding scatterplot geom (layer1) and smoothing geom (layer2).
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() 
