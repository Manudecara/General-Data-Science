#Analysing Bendford's Law with the 'benford.analysis' package

install.packages("benford.analysis")
library(benford.analysis)
library(quantmod)

#lets see if it can spot fraudulent credit card users
data(corporate.payment)
bfd.cp <- benford(corporate.payment$Amount)

plot(bfd.cp)

suspects <- getSuspects(bfd.cp, corporate.payment)
suspects

#is it present in finance? 
getSymbols('AAPL', src = 'yahoo', from = "2010-01-01", to = '2020-01-01')

digits <- extract.digits(AAPL[,4], number.of.digits = 1, sign = "positive",
                   discrete = TRUE, round = 1)

benfordlaw <- benford(digits[,2], number.of.digits = 1, sign = "positive",
                          discrete = TRUE, round = 1)

benfordlaw
plot(benfordlaw)

