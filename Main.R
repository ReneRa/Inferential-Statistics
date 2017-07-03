###Inferential Statistics###

install.packages("lmtest")
library("corrplot")
library(lmtest)
library(rms)

install.packages("rms")

rm(list = ls())
juices = read.csv("JuiceDataset.csv", header = TRUE, sep = ";", row.names= 1)
fruits = list("Apple","Orange","Pineapple","Mango","Grape","Maracuja","Peach","Pear","Banana","Carrot","Lemon","Others")
common_fruits = list("Apple","Orange","Pineapple","Mango","Maracuja","Banana")
other_fruits = c("Kiwi","Tomato","Grapefruit","Rosa_Camina","Guava","Apricot","Mandarin","Papaia","Raspberry","Blueberry","Strawberry","Cherries")
bincoms = list("BinApplePineapple", "BinAppleBanana", "BinAppleCarrot", "BinAppleOthers", "BinOrangeOthers", "BinOrangePineapple", "BinBananaPeach", "BinPearApple", "BinPearOrange")
triples = list("AppOrangPina", "AppOrangBana", "AppOrangMan", "AppOrangMara", "OrangPinaMan", "OrangManMara")

#Create Other Category
juices$Cherries <- rowSums(juices[other_fruits])
drops <- other_fruits[other_fruits!="Cherries"]
juices <- juices[ , !(names(juices) %in% drops)]
names(juices)[names(juices) == "Cherries"] <- "Others" 


# Scaling
juices[,13:24] <- juices[,13:24]/juices$Fruit_Proportion
try(if(all(abs(rowSums(juices[,13:24])-1)<0.001)) stop("Threshold for row sums"))

# Dummy variables for fruits
juices[,13:24] <- ceiling(juices[,13:24])
paircount<-ceiling(juices[,13:24])

#Combinations
juices$BinApplePineapple <- juices$Apple * juices$Pineapple
juices$BinAppleBanana <- juices$Apple * juices$Banana
juices$BinAppleCarrot <- juices$Apple * juices$Carrot
juices$BinAppleOthers <- juices$Apple * juices$Others
juices$BinOrangeOthers <- juices$Orange * juices$Others
juices$BinOrangePineapple <- juices$Orange * juices$Pineapple
juices$BinBananaPeach <- juices$Banana * juices$Peach
juices$BinPearApple <- juices$Pear * juices$Apple
juices$BinPearOrange <- juices$Pear * juices$Orange

juices$AppOrangPina <- (juices$Apple * juices$Pineapple *juices$Orange)^(1/3)
juices$AppOrangBana <- (juices$Apple *juices$Orange * juices$Banana)^(1/3)
juices$AppOrangMan <- (juices$Apple *juices$Orange * juices$Mango)^(1/3)
juices$AppOrangMara <- (juices$Apple *juices$Orange * juices$Maracuja)^(1/3)
juices$OrangPinaMan <- (juices$Pineapple *juices$Orange * juices$Mango)^(1/3)
juices$OrangManMara <- (juices$Pineapple *juices$Maracuja * juices$Mango)^(1/3)


#Assumption Testing
cor(juices[,2:24])
corrplot(as.matrix(cor(juices[,2:24])), is.corr = FALSE, method = "color",
         order="hclust", type = "upper")

par(mfrow = c(4, 6))
for (i in juices[2:24]){
  plot(i,juices$Median)
  abline(lm(juices$Median~i), col="red") # regression line (y~x)

}

#Linear model
copy_juices <- juices
f<-paste(fruits[!fruits %in% common_fruits], collapse="+") #[ fruits %in% common_fruits]
c<-paste(triples,collapse = "+")
c<-paste(f,c,sep="+")
linear <- lm(as.formula(paste("MinMaxPolarisation ~ ",c,sep="")), data=copy_juices)
linear<- lm(MinMaxPolarisation~ Sugar+Fruit_Proportion + AppOrangBana + OrangPinaMan, data = copy_juices)
#waldtest(linear)
summary(linear)

#Logit model
logit <- glm(as.formula(paste("Average_Recom ~ ",c,sep="")), family=binomial(link="logit"), data=juices)
summary(logit)
waldtest(logit, "BinApplePineapple", test = "Chisq")
f<-paste(fruits[fruits!="Others"], collapse="+")
f<-paste(fruits, collapse="+")
logit <- glm(as.formula(paste("Average_Recom ~ Fruit_Proportion+Price+",f,sep="")), family=binomial(link="logit"), data=juices)
logit2 <- glm("Average_Recom ~ ", family=binomial(link="logit"), data=juices$Average_Recom)
lrtest(logit, logit2)
waldtest(logit, "Price", test = "Chisq")

logit_lrm<-lrm(as.formula(paste("Average_Recom ~ ",f,sep="")),data=juices)

# logit_p <- glm(as.formula("Average_Recom ~ Pineapple + Banana + Apple"), family=binomial(link="logit"), data=juices)
# xweight<- seq(,1,0.028)
# yweight<-predict(logit_p,list(Pineapple=juices$Pineapple, Banana = juices$Banana, Apple=juices$Apple),type="response")
# yweight<-sort(yweight)
# length(xweight)
# #yweight<-predict(logit_p,list(Fruit_Proportion=juices$Fruit_Proportion, Pineapple = xweight, Banana = xweight, Apple=xweight, Orange=xweight, Pear=xweight, Mango=xweight, Grape=xweight,Maracuja=xweight, Peach=xweight, Carrot=xweight, Lemon=xweight), type="response")
# plot(juices$Fruit_Proportion,juices$Average_Recom, pch=16)
# lines(xweight,yweight)
# summary(logit_p)

#Probit model
probit <- glm(as.formula(paste("Average_Recom ~ Fruit_Proportion+",f,sep="")), family=binomial(link="probit"), data=juices)

#Model Validity Test
lrtest()
waldtest()


#Multinomial Logit Model
library(foreign)
library(nnet)
library(stargazer)
MultinomLogit = multinom(Rounded_Average ~ Price + Fruit_Proportion + Fresh_Juice, data=juices)
stargazer(MultinomLogit, type="text", out="multi1.htm")
multi1.rrr = exp(coef(MultinomLogit))


#notes:
#http://www.princeton.edu/~otorres/LogitR101.pdf

# Research question
# 1. Impact of each fruit 
# 2. 
# 3.



#Frequency of pairs
# for (i in 1:10){
#   ix=i+1
#   for (j in ix:11){
#     jx=j+1
#     for (k in jx:12){
#       print(paste(names(paircount[i]), names(paircount[j]),names(paircount[k]), sum(paircount[i]*paircount[j]*paircount[k]), min(sum(paircount[i]),sum(paircount[j]),sum(paircount[k])), sep=" "))
#     }
#   }
# }
# 
# for (i in 1:12)
#   print(paste(names(paircount[i]), sum(paircount[i]), sep=" "))'