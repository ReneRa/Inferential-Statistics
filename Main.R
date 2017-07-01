###Inferential Statistics###


rm(list = ls())
juices = read.csv("JuiceDataset.csv", header = TRUE, sep = ";", row.names= 1)

#Combine variables
juices$Cherries <- rowSums(juices[c("Kiwi","Tomato","Grapefruit","Rosa_Camina","Guava","Apricot","Mandarin","Papaia","Raspberry","Blueberry","Strawberry","Cherries")])
drops <- c("Kiwi","Tomato","Grapefruit","Rosa_Camina","Guava","Apricot","Mandarin","Papaia","Raspberry","Blueberry","Strawberry")
juices <- juices[ , !(names(juices) %in% drops)]
names(juices)[names(juices) == "Cherries"] <- "Others" 

# Scaling
juices[,13:24] <- juices[,13:24]/juices$Fruit_Proportion
try(if(all(abs(rowSums(juices[,13:24])-1)<0.001)) stop("Threshold for row sums"))

# Dummy variables for fruits
#juices[,13:24] <- ceiling(juices[,13:24])

#Logit model
logit <- glm(juices$Average_Recom ~ juices$Price + juices$Fruit_Proportion + juices$Fresh_Juice, family=binomial(link="logit"), data=juices)

#Probit model
probit <- glm(juices$Average_Recom ~ juices$Price + juices$Fruit_Proportion + juices$Fresh_Juice, family=binomial(link="probit"), data=juices)

#Model Validity Test
library(lmtest)
lrtest()
waldtest()


#Multinomial Logit Model
library(foreign)
library(nnet)
library(stargazer)
MultinomLogit = multinom(Rounded_Average ~ Price + Fruit_Proportion + Fresh_Juice, data=juices)
stargazer(MultinomLogit, type="html", out="multi1.htm")
multi1.rrr = exp(coef(MultinomLogit))


#notes:
#http://www.princeton.edu/~otorres/LogitR101.pdf

# Research question
# 1. Impact of each fruit 
# 2. 
# 3.
