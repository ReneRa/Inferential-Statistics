########################################################
#                  Data Transformations                #
########################################################

createOtherCategory<-function(data){
  data$Cherries <- rowSums(data[other_fruits])
  drops <- other_fruits[other_fruits!="Cherries"]
  data <- data[ , !(names(data) %in% drops)]
  names(data)[names(data) == "Cherries"] <- "Others"
  return(data)
}

scalingFruits<-function(data){
  data[,13:24] <- data[,13:24]/data$Fruit_Proportion
  try(if(all(abs(rowSums(data[,13:24])-1)<0.001)) stop("Threshold for row sums"))
  return(data)
}

dummyFruits <-function(data){
  data[,13:24] <- ceiling(data[,13:24])
  return(data)
}

fruitCombiations <- function(data, x,y){
# Pairs
data$P_ApplePineapple <- data$Apple * data$Pineapple *x
data$P_AppleBanana <- data$Apple * data$Banana *x
data$P_AppleCarrot <- data$Apple * data$Carrot *x
data$P_AppleOthers <- data$Apple * data$Others *x
data$P_OrangeOthers <- data$Orange * data$Others *x
data$P_OrangePineapple <- data$Orange * data$Pineapple *x
data$P_BananaPeach <- data$Banana * data$Peach *x
data$P_PearApple <- data$Pear * data$Apple *x
data$P_PearOrange <- data$Pear * data$Orange *x
data$P_CarrotLemon <- data$Carrot * data$Lemon *x
data$P_PeachApple <- data$Peach * data$Apple *x

# Triples
data$T_AppOrangPina <- (data$Apple * data$Pineapple *data$Orange)*y
data$T_AppOrangBana <- (data$Apple *data$Orange * data$Banana)*y
data$T_AppOrangMan <- (data$Apple *data$Orange * data$Mango)*y
data$T_AppOrangMara <- (data$Apple *data$Orange * data$Maracuja)*y
data$T_OrangPinaMan <- (data$Pineapple *data$Orange * data$Mango)*y
data$T_OrangManMara <- (data$Pineapple *data$Maracuja * data$Mango)*y
return(data)
}



########################
# Backlog

# xweight<- seq(,1,0.028)
# yweight<-predict(logit_p,list(Pineapple=juices$Pineapple, Banana = juices$Banana, Apple=juices$Apple),type="response")
# yweight<-sort(yweight)
# length(xweight)
# #yweight<-predict(logit_p,list(Fruit_Proportion=juices$Fruit_Proportion, Pineapple = xweight, Banana = xweight, Apple=xweight, Orange=xweight, Pear=xweight, Mango=xweight, Grape=xweight,Maracuja=xweight, Peach=xweight, Carrot=xweight, Lemon=xweight), type="response")
# plot(juices$Fruit_Proportion,juices$Average_Recom, pch=16)
# lines(xweight,yweight)
# summary(logit_p)


# #Frequency of pairs
# paircount<-ceiling(juices[,13:24])
# for (i in 1:10){
#   ix=i+1
#   for (j in ix:11){
#     jx=j+1
#     for (k in jx:12){
#       print(paste(names(paircount[i]), names(paircount[j]),names(paircount[k]), sum(paircount[i]*paircount[j]*paircount[k]), sum(paircount[i]*paircount[j]*paircount[k]*juices$Average_Recom), min(sum(paircount[i]),sum(paircount[j]),sum(paircount[k])), sep=" "))
#     }
#   }
# }
# 
# for (i in 1:10){
#   ix=i+1
#   for (j in ix:11){
#       print(paste(names(paircount[i]), names(paircount[j]), sum(paircount[i]*paircount[j]), sum(paircount[i]*paircount[j]*juices$Average_Recom)), sep=" ")
#   }
# }
# 
# for (i in 1:12)
#   print(paste(names(paircount[i]), sum(paircount[i]), sep=" "))'

#notes:
#http://www.princeton.edu/~otorres/LogitR101.pdf

# Research question
# 1. Impact of each fruit 
# 2. 
# 3.


# par(mfrow = c(4, 6))
# for (i in juices[2:24]){
#   plot(i,juices$Median)
#   abline(lm(juices$Median~i), col="red") # regression line (y~x)
#   
# }