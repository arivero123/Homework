# Homework
#Loading necessary packages
library(stats)
library(gendata)
library(pwr)

#Q1-----
#how to create r markdown
#file -> new file -> r markdown -> label it 

#Using loop for r simulation
TTestPVS <- numeric(1000) 

#Creating a loop
set.seed(123)
for (i in 1:1000){
  y1 <- rnorm(n = 100, mean = 100, sd = 15) 
  y2 <- rnorm(n = 100, mean = 100, sd = 15)
  Group <- rep(c("Group A", "Group B"), each = 100) 
  DV <- c(y1, y2)
  data <- data.frame(Group, DV)
  TTestResult <- t.test(DV ~ Group, data = data)
  TTestPVS[i] <- TTestResult$p.value #Pvalues
}

#Type 1 error rate is rejecting the null hypothesis, when there is no significant difference between groups
#Type 1 error occurs when: a significant p-value is generated

#Computing how many times a significant p-value was generated
TTestSimSum <- sum(TTestPVS <= .05) #how many times there are significant differences when there shouldn't be one because they are the exact same values   

#A significant p-value was generated 41 times out of 1,000 
TTestSimSum/1000

#Expect to see significant results 4.1% of the time. 


#Q2A-----

#Using loop for r simulation
CorrPVS <- numeric(1000) 
CorrCoeff <- numeric(1000) 

#Creating a loop
set.seed(123)

for (i in 1:1000){
  d1 <- genmvnorm(cor = .3, k = 2, n = 50) #k = number of variables in data set
  CorrResult <- cor.test(d1$X1, d1$X2)
  CorrPVS[i] <- CorrResult$p.value
  CorrCoeff[i] <- CorrResult$estimate # gives correlation coefficient 
}

CorrSimSum <- sum(CorrPVS <= .05) 

CorrSimSum/1000

#The correlation between these two variables is significant 59% of the time

#Q2B-----

#Histogram below: 

hist(CorrCoeff, breaks = 20)

#Q2C-----

#Mean for the correlation coefficients:
mean(CorrCoeff)

#The mean correlation coefficient is .30

#Q2D-----

set.seed(123)

#Creating a loop
CorrPVSQD <- numeric(1000) 
CorrCoeffQD <- numeric(1000) 


for (i in 1:1000){
  d1 <- genmvnorm(cor = .15, k = 2, n = 100) 
  CorrResultQD <- cor.test(d1$X1, d1$X2)
  CorrPVSQD[i] <- CorrResultQD$p.value
  CorrCoeffQD[i] <- CorrResultQD$estimate
}

QDSimPsum<- sum(CorrPVSQD <= .05) 

QDSimPsum/1000

#Power = .305


#Q2E--------
install.packages("pwr")
library(pwr)

CorrPVSQE <- numeric(1000) 
CorrCoeffQE <- numeric(1000) 

#Creating a loop
set.seed(123)

for (i in 1:1000){
  d1 <- genmvnorm(cor = .15, k = 2, n = 348) 
  CorrResultQE <- cor.test(d1$X1, d1$X2)
  CorrPVSQE[i] <- CorrResultQE$p.value
  CorrCoeffQE[i] <- CorrResultQE$estimate
}

QEPSimPsum <- sum(CorrPVSQE <= .05) 
QEPSimPsum/1000

set.seed(123) 
pwr.r.test(n = NULL, r = .15, sig.level = .05, power = .80)

#n = 348 or 345.70 using pwr package. 
