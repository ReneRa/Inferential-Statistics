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

