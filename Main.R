###Inferential Statistics###

install.packages("globals")
library("corrplot")
library(lmtest)
library(rms)
library(car)
library(globals)

install.packages("rms")

rm(list = ls())
source("data_transformations.R")
source("Assumption_Testing.R")
juices = read.csv("JuiceDataset.csv", header = TRUE, sep = ";", row.names= 1)

#Lists
fruits = list("Apple","Orange","Pineapple","Mango","Grape","Maracuja","Peach","Pear","Banana","Carrot","Lemon","Others")
common_fruits = list("Apple","Orange","Pineapple","Mango","Maracuja","Banana")
other_fruits = c("Kiwi","Tomato","Grapefruit","Rosa_Camina","Guava","Apricot","Mandarin","Papaia","Raspberry","Blueberry","Strawberry","Cherries")
pairs = list("BinApplePineapple", "BinAppleBanana", "BinAppleCarrot", "BinAppleOthers", "BinOrangeOthers", "BinOrangePineapple", "BinBananaPeach", "BinPearApple", "BinPearOrange")
triples = list("AppOrangPina", "AppOrangBana", "AppOrangMan", "AppOrangMara", "OrangPinaMan", "OrangManMara")

#New Targets
#juices$Recom5 <- ifelse(juices$Average>=5,1,0)

#Data Transformations
juices<-createOtherCategory(juices)
juices_scaled <- scalingFruits(juices)
juices_dummy <- dummyFruits(juices)
juices<-fruitCombinations(juices,10,100)
juices_scaled<-fruitCombinations(juices_scaled,10,100)
juices_dummy<-fruitCombinations(juices_dummy,1,1)

##########################################################
#                       Modeling                         #
##########################################################

# Fruit Combinations
#########################

#Linear model
copy_juices <- juices_scaled
copy_juices$t<-juices$Added_Sugar * juices$Sugar
copy_juices$Carrot2 <- copy_juices$Carrot^2*10
model1 = "T_AppOrangBana + T_AppOrangPina + T_OrangPinaMan + P_CarrotLemon+ P_PearApple"
model2 = "T_AppOrangBana + T_AppOrangPina + T_OrangPinaMan + Carrot + Sugar + P_PearApple+ Kcal"
model3 = "T_AppOrangBana + T_AppOrangPina + T_OrangPinaMan + Carrot + Added_Sugar + Fresh_Juice + P_PearApple"
linear <- lm(as.formula(paste("Avg_Rating ~ ",model3,sep="")), data=copy_juices)
summary(linear)

f<-paste(fruits[!fruits %in% common_fruits], collapse="+") #[ fruits %in% common_fruits]
f<-""
#c<-paste(bincoms,collapse = "+")
#c<-paste(f,c,sep="+")
#linear <- lm(as.formula(paste("Average~  AppOrangBana + AppOrangPina + OrangPinaMan + BinCarrotLemon+BinPearApple ",f,sep="")), data=copy_juices)
#linear2 <-lm(Average~, data=copy_juices)
#linear<- lm(Average~  Fruit_Proportion+ AppOrangBana + OrangPinaMan, data = copy_juices)
waldtest(linear)
summary(linear)

#Logit model
logit <- glm(as.formula(paste("Average_Recom ~ ",c,sep="")), family=binomial(link="logit"), data=juices)
summary(logit)
waldtest(logit, "BinApplePineapple", test = "Chisq")
f<-paste(fruits[!fruits %in% common_fruits], collapse="+")
f<-paste(triples, collapse="+")
f<-"Carrot + Sugar+  BinPearApple+ Mango"
f<-""
copy_juices<-juices
copy_juices$Sugar2 = juices$Sugar^2
copy_juices$SugarPerKcal = juices$Sugar/juices$Kcal
copy_juices$Fruit_Proportion2 = juices$Fruit_Proportion^2
#logit <- glm(as.formula(paste("Average_Recom~ Sugar + Sugar2 + BinPearApple + BinCarrotLemon",f,sep="")), family=binomial(link="probit"), data=copy_juices)
logit <- glm(Average_Recom~ AppOrangBana+ OrangPinaMan + AppOrangPina + Kcal + Sugar + BinCarrotLemon+BinPeachApple, family=binomial(link="probit"), data=copy_juices)
summary(logit)
#logit2 <- glm("Recom5 ~ ", family=binomial(link="logit"), data=juices$Average_Recom)
#lrtest(logit, logit2)
waldtest(logit, test = "Chisq")

logit_lrm<-lrm(as.formula(paste("Average_Recom ~ ",f,sep="")),data=juices)

 logit_p <- glm(as.formula("Average_Recom ~ Pineapple + Banana + Apple"), family=binomial(link="logit"), data=juices)


#Probit model
probit <- glm(as.formula(paste("Average_Recom ~ Fruit_Proportion+",f,sep="")), family=binomial(link="probit"), data=juices)

#Model Validity Test
lrtest()
waldtest()



##################
# Dimitris Models

##############
juices$PriceAdjAverage = juices$Average/juices$Price
juices$PriceAdjMinMax = juices$MinMaxPolarisation/juices$Price
juices$PriceAdjVariance = juices$Variance/juices$Price

juices$logPrice = log(juices$Price)
juices$logAverage = log(juices$Average)

juices$maxRating <- apply(evaluation[,1:5], 1, max)
juices$minRating <- apply(evaluation[,1:5], 1, min)
juices$sqDifMinMax = (juices$maxRating - juices$minRating)^2
juices$DifMinMax = (juices$maxRating - juices$minRating)^2

juices$AdjPrice = sapply(juices$Price, function(x){return(min(x,2.5))})
juices$priceAdjAvg = juices$Average + 0.3*juices$Price


# #Multinomial Logit Model
# library(foreign)
# library(nnet)
# library(stargazer)
# MultinomLogit = multinom(Rounded_Average ~ Price + Fruit_Proportion + Fresh_Juice, data=juices)
# stargazer(MultinomLogit, type="text", out="multi1.htm")
# multi1.rrr = exp(coef(MultinomLogit))
