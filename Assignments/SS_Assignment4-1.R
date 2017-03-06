library(readr)
library("klaR")
library("ade4")
house_votes_84 <- read_csv("~/Downloads/Education/Spring 17/R for Data Scientists/Assignments/house-votes-84.txt")
View(house_votes_84)
dim(house_votes_84)
myData <- house_votes_84
myData = as.data.frame(unclass(house_votes_84))
head(myData)
myData$republican<-NULL
myDataClean <- na.omit((myData))
dim(myDataClean)
length(myDataClean)

for ( i in 1 : length(myDataClean)){
  myDataClean[,i] = as.factor(myDataClean[,i])
}

head(myDataClean)

dataBinMM = data.frame(model.matrix(~.,data = myDataClean)[,-1])
head(dataBinMM)

dataDistBinaryMM <- dist(dataBinMM,method = "binary")
dataDistEucMM <- dist(dataBinMM,method = "euclidian")
fitMMBi <- hclust(d=dataDistBinaryMM, method ="ward.D2")
fitMMEu <- hclust(d=dataDistEucMM, method ="ward.D2")

plot(fitMMBi)

xMMbi = cutree(fitMMBi,2)
y = myDataClean[xMMbi ==1,]
summary(y)
y=myDataClean[xMMbi == 2,]
summary(y)

xMMEu=cutree(fitMMEu,2)
y=myDataClean[xMMEu == 1,]
summary(y)
y=myDataClean[xMMEu == 2,]
summary(y)

table(xMMbi,xMMEu)

newBinaryData = acm.disjonctif(myDataClean)
head(newBinaryData)

distBinaryade4 = dist(newBinaryData, method = "binary")
distEucade4 = dist(newBinaryData, method = "euclidian")
fitade4Bi = hclust(d=distBinaryade4,method="ward.D2")
fitade4Eu = hclust(d=distEucade4,method="ward.D2")

xade4bi=cutree(fitade4Bi,2)
y=myDataClean[xade4bi == 1,]
summary(y)
y=myDataClean[xade4bi == 2,]
summary(y)

xade4eu=cutree(fitade4Eu,2)
y=myDataClean[xade4eu == 1,]
summary(y)
y=myDataClean[xade4eu == 2,]
summary(y)

table(xade4bi,xade4eu)


km = kmodes(myDataClean, 2)

table(xade4bi,km$cluster)

table(xade4eu,km$cluster)

table(xMMbi,km$cluster)

table(xMMEu,km$cluster)