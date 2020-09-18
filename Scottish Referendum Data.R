#set working directory
setwd("~/Q-Step - Year 1 ")

#load libraries
library(tidyverse)
library(foreign)
library(psych)
library(dplyr)
library(mosaic)
library(sjmisc)
library(gmodels)
library(vcd)
library(arsenal)


#load data
scotref <- read.dta("~/Downloads/scotref_2014.dta")

#Remove '99' values
scotref$polInterestRefW1[scotref$polInterestRefW1>11] <- NA
scotref$polInterestUKW1[scotref$polInterestUKW1>11] <- NA
scotref$polInterestScotW1[scotref$polInterestScotW1>11] <- NA
scotref$polInterestForeignW1[scotref$polInterestForeignW1>11] <- NA

#remove NA's
scotref <- na.omit(scotref)
summary(scotref)

#Question 1

# How did interest in the Scottish referendum vary within and between regions in
# Scotland? (5 points)

# - Calculate an appropriate measure of central tendency and dispersion for
# interest in the Scottish referendum for each region. Report a table and describe
# its contents. 

#'describeby' function will return a number of obvervations depending on the columns inserted. 
#the mean and standard deviation are the necessary measures 
#to understand the interest in the Scottish referendum by region.
describeBy(scotref$polInterestRefW1, scotref$scotgeogW1)
mean(scotref$polInterestRefW1)
sd(scotref$polInterestRefW1)

#Question 2

# Is the proportion who said they would be very likely to vote in the referendum (or
# had already voted by post at the time of their interview) in the Glasgow region
# significantly different from the national average? (5 points)

# - Use an appropriate statistical test to test this hypothesis regarding the
# proportion planning or had already voted in Glasgow compared with the
# national average, specifying an alpha level of 0.10. State your hypothesis and
# report the results of your test.

#In order to compare the votes between glasgow and the national average the two must be separated.
scotref$regions[scotref$scotgeogW1=="Central"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="Lothians"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="Highlands & Islands"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="Mid-Scotland & Fife"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="North East Scotland"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1== "South Scotland"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="West Scotland"] = "NationalAverage"
scotref$regions[scotref$scotgeogW1=="Glasgow"] = "Glasgow"

#turn the types of votes into numeric form in order to be measured
scotref$vote_type <- as.numeric(scotref$scotReferendumTurnoutW1)

#remove the 7th option because it's by people who 'dont know' whether they voted
scotref <- subset(scotref, scotref$vote_type!="7")

#conduct the t-test
t.test(vote_type ~ regions, data = scotref)


#Question 3

# Is the mean interest in international politics higher for people aged 60 and over
# compared with the national average? (5 points)

# - Use an appropriate statistical test to test a hypothesis about the mean for
# people aged 60 and over, specifying an alpha level of 0.01. State your
# hypothesis and report the results of your test.


#conduct t.test
scotref$newage <- cut(scotref$ageW1, breaks=c(15, 59, Inf), labels=c("Young", "Old"))
t.test(polInterestForeignW1 ~ newage, data = scotref)


#Question 4
# Do Scottish Conservative voters think London gets more or less than its fair share
# of UK government spending? (10 points)

# - Produce a cross tabulation between whether London gets its fair share of UK
# government spending and party voted for at the 2010 general election.
# Describe the table in your text and report the result of an appropriate test to 
# determine whether there is a relationship between the two variables in your
# table. Comment on whether your data meet the assumptions required to
# conduct the test and present only a final cross-tabulation that meets these
# assumptions.

#create table of convservatives only and their opinions
scotref$party[scotref$qvoteW1=="Labour Party"] = "National Average"
scotref$party[scotref$qvoteW1=="Liberal Democrats"] = "National Average"
scotref$party[scotref$qvoteW1=="Scottish National Party"] = "National Average"
scotref$party[scotref$qvoteW1=="British National Party (BNP)"] = "National Average"
scotref$party[scotref$qvoteW1=="Did not vote"] = "National Average"
scotref$party[scotref$qvoteW1=="Don't know"] = "National Average"
scotref$party[scotref$qvoteW1=="United Kingdom Independence Party (UKIP)"] = "National Average"
scotref$party[scotref$qvoteW1=="Some other party"] = "National Average"
scotref$party[scotref$qvoteW1=="Green Party"] = "National Average"
scotref$party[scotref$qvoteW1=="Respect"] = "National Average"
scotref$party[scotref$qvoteW1=="Plaid Cymru"] = "National Average"
scotref$party[scotref$qvoteW1=="Conservative Party"] = "Conservative Party"

#create cross-tabulation
x <- tableby(scotref$party ~ scotref$londonFairShareW1)
summary(x, title = "Opinions of Conservative voters on whether London gets its fair share of UK government spending")

#carry out two-way chi squared test
scotref$opinion <- as.numeric(scotref$londonFairShareW1)
x2 <- table(scotref$party, scotref$opinion)
chisq.test(x2)

#Question 5 

# Are men more interested in Scottish politics than women? (15 points)

# - Use an appropriate statistical test to test this hypothesis and demonstrate
# whether your data meet the assumptions to conduct the test. State your
# hypothesis and report the results of your test in a table. Describe whether the
# data meet the assumptions required for the test using the data in the table and
# using up to two separate plots. 

#create table with Genders and their interst on Scottish politics
describeBy(scotref$polInterestScotW1, scotref$profile_genderW1)

#conduct t.test
scotpol_gender <- data.frame(scotref$profile_genderW1, scotref$polInterestScotW1)
names(scotpol_gender) <- c('Gender', 'IntScotPol')
t.test(scotpol_gender$IntScotPol ~ scotpol_gender$Gender)

#create graph to further display the results
means_gender_scot <- data.frame(x = c("Male", "Female"), y= c(7.609375, 7.096369))
names(means_gender_scot) <- c('Genders', 'Means Scot')
Gender_Scot <- ggplot(scotpol_gender, aes(x = scotpol_gender$IntScotPol, colour = scotpol_gender$Gender)) + geom_density() +
  geom_vline(aes(xintercept = means_gender_scot$`Means Scot`, colour = means_gender_scot$Genders), data = means_gender_scot, linetype = "dashed", size = 0.5)
Gender_Scot


#Question 6

# Are the same people interested in different aspects of politics, specifically UK
# politics, Scottish politics and international politics? (5 points)

# - Explore the descriptive strength of the relationship between interest in UK,
# Scottish and international politics. Report and describe an appropriate table or
# plot.


#create table 
comparison_table <- data.frame(scotref$profile_genderW1, scotref$ageW1, scotref$polInterestUKW1, scotref$polInterestScotW1, scotref$polInterestForeignW1)
names(comparison_table) <-c("Gender","Age","Interest UK","Interest Scot","Interest IR")
#separate ages into two groups: 'young and old'
comparison_table$Age <- cut(comparison_table$Age, breaks=c(10, 50, 90), right = FALSE, labels = c('Young', 'Old'))

#gather all their means in order to construct final table
mean(comparison_table$`Interest UK`[comparison_table$Gender== 'Male'])
mean(comparison_table$`Interest UK`[comparison_table$Gender== 'Female'])
mean(comparison_table$`Interest Scot`[comparison_table$Gender== 'Male'])
mean(comparison_table$`Interest Scot`[comparison_table$Gender== 'Female'])
mean(comparison_table$`Interest IR`[comparison_table$Gender== 'Male'])
mean(comparison_table$`Interest IR`[comparison_table$Gender== 'Female'])

mean(comparison_table$`Interest UK`[comparison_table$Age== 'Young'])
mean(comparison_table$`Interest UK`[comparison_table$Age== 'Old'])
mean(comparison_table$`Interest Scot`[comparison_table$Age== 'Young'])
mean(comparison_table$`Interest Scot`[comparison_table$Age== 'Old'])
mean(comparison_table$`Interest IR`[comparison_table$Age== 'Young'])
mean(comparison_table$`Interest IR`[comparison_table$Age== 'Old'])

table1 <- data.frame(x=c("Male", "Female"), y=c(7.140234, 6.509189), y1=c(7.609375, 7.096369), 
                     y2=c(6.497656, 5.580905))
names(table1) <- c('Genders & Ages', 'Mean interest in UK politics', 'Means interest in Scottish politics', 'Means interest in foreign politics')

table2 <- data.frame(x=c('Young', 'Old'), y3=c(6.586207, 7.068032), y4=c(7.143829,7.563587),
                     y5=c(5.877495, 6.235408))
names(table2) <- c('Genders & Ages', 'Mean interest in UK politics', 'Means interest in Scottish politics', 'Means interest in foreign politics')

final_table <- rbind(table1, table2)
final_table

#Question 7

# Does age predict interest in UK politics? (25 points)

# - State your hypothesis and report a statistical model in a table. The model
# should enable you to explain variation in interest in UK politics using single
# year of age as an explanatory variable.

# - Describe the findings from your model relating to your hypothesis.

# - Use your model to predict interest in UK politics for an individual aged 85 and
# explain whether this is an appropriate prediction to make.

# - Check your model for one assumption of your residual values using an
# appropriate test.

# - Comment on the broader limitations of your model

#test for correlation
cor(scotref$polInterestUKW1, scotref$ageW1, method= c("pearson"))

#create table
ageinterestUK <- data.frame(y=scotref$ageW1, x=scotref$polInterestUKW1)
names(ageinterestUK) <- c('Age', "InterestUK")


#create table of means
ageinterestUK <- aggregate(ageinterestUK[,2], list(ageinterestUK$Age), mean)
names(ageinterestUK) <- c('Age', "MeanInterestUK")
cor(ageinterestUK$MeanInterestUK, ageinterestUK$Age)

#produce scatterplot
ggplot(data= ageinterestUK, mapping = aes(x=ageinterestUK$Age, y=ageinterestUK$MeanInterestUK,
                                          labs="Age ~ MeanInterestUK")) + geom_point()

#trace line
ggplot(data= ageinterestUK, mapping = aes(x=ageinterestUK$Age, y=ageinterestUK$MeanInterestUK, 
                                          labs="Age ~ MeanInterestUK")) + geom_point() + geom_smooth(method=lm, se=FALSE)

#decribe model results
model <- lm(ageinterestUK$MeanInterestUK ~ ageinterestUK$Age)

summary(model)

#do regression equation to predict age for 85 year old (y = b0 + b1xi)
6.02 + 0.017*85 
#predictive answer: 7.53










