setwd('~/R practice')

#Obtaining the best cereal overall

#Downloading the dataset
cereal <- read.csv("~/Downloads/cereal.csv")

#Exploring the data
summary(cereal)
sort(cereal$calories, decreasing = TRUE)
df_calories <- data.frame(cereal$name, cereal$calories)

#1st attempt
clean_cereal <- data.frame(cereal$name, 
                           cereal$calories > 50 & cereal$calories < 100,
                           cereal$protein > 2.5 & cereal$protein < 5.5,
                           cereal$fat < 2,
                           cereal$sodium > 100 & cereal$sodium < 200,
                           cereal$fiber > 2,
                           cereal$carbo > 10 & cereal$carbo < 20,
                           cereal$sugars > 3 & cereal$sugars < 10,
                           cereal$potass > 100,
                           cereal$vitamins > 25,
                           cereal$rating > 40)
#unorganised
#hard to read
#learning
#too big


#2nd attempt
cereal$requisites <- 0

cereal$requisites[((cereal$calories < 100) &
                   (cereal$protein > 2) &
                   (cereal$fat < 2) &
                   (cereal$sodium > 100 & cereal$sodium < 250) &
                   (cereal$fiber > 2) &
                   (cereal$carbo > 10 & cereal$carbo < 20) &
                   (cereal$sugars < 10) &
                   (cereal$potass > 40) &
                   (cereal$vitamins > 10) &
                   (cereal$rating > 30))] <- 1

#clean
#organised


