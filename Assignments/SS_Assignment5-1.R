#sample mean
mean(lr.model_1$residuals^2)

summary(spiritwt_data$Wght)

#glm - another way of building the model

#create model and feed it to cvm function to do LOOCV 
#delta contains the cross validation

#take model that gives lowest msc

#take the first value of delta since it is more non-weighted


library(readr)
auto_mpg <- read_delim("~/Downloads/Education/Spring 17/R for Data Scientists/Assignments/auto-mpg.data-original",
                       "\t", escape_double = FALSE, col_names = FALSE,
                       trim_ws = TRUE)
View(auto_mpg)
head(auto_mpg)
summary(auto_mpg)
dim(auto_mpg)
colnames(auto_mpg) = c ("MPG","Cylinder","Displacement","HorsePower","Weight","Acc","ModelYr","Origin","CarName")
myData <- auto_mpg
myData <- na.omit(myData)
dim(auto_mpg)
dim(myData)
lr.model.1 <- lm(MPG~.,data=myData)
summary(lr.model.1)

#try the model using train and test
myData = myData[,-8]
train_data = sample(392,266) #68%
lr.model.2 <- lm(MPG~., data=myData, subset=train_data)
summary(lr.model.2)
mean(lr.model.2$residuals^2) #12.47473
summary(myData$MPG)  #0.66% of average MPG, so that seems to be okay

mean( (MPG - predict(lr.model.2, myData) ) [-train_data]^2) #9.9745

#try doing with LOOCV
library(boot)
lr.model.boot = glm(MPG~., data=myData)
summary(lr.model.boot)

cv.error.boot = cv.glm(myData,lr.model.boot)

cv.error.boot$delta #12.08526

cv.error.kcv <- cv.glm(myData,lr.model.boot,K=10)

cv.error.kcv$delta #12.20015 - 14.8%