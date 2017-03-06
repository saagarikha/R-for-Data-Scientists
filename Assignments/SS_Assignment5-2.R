library(readr)
student_por <- read_delim("~/Downloads/Education/Spring 17/R for Data Scientists/Assignments/student/student-por.csv", ";", escape_double = FALSE, trim_ws = TRUE)

myData_grade <- as.data.frame(student_por)

lr.model.1 <- lm(myData_grade$G3~.,data=myData_grade)

summary(lr.model.1)

#train and test
train_data = sample(649,441) #68%

lr.model.2 <- lm(G3~., data=myData_grade, subset=train_data)
summary(lr.model.2)

mean(lr.model.2$residuals^2) #1.26 - about 9% of data falls 

#prediction
mean( (myData_grade$G3 - predict(lr.model.2, myData_grade) ) [-train_data]^2) #2.0593

#try with LOOCV
lr.model.boot = glm(G3~., data=myData_grade)
summary(lr.model.boot)

cv.error.boot = cv.glm(myData_grade,lr.model.boot)

cv.error.boot$delta #1.686186

cv.error.kcv <- cv.glm(myData_grade,lr.model.boot,K=10)

cv.error.kcv$delta #1.675 - 9%
