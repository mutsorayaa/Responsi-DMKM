#Library yang diugunakan
library(tidyverse)
library(caret)
library(e1071)
library(ggplot2)

#Input data
data <- read.csv("D:/3SD1/DMKM/UTS/seeds_dataset.txt", header=F, sep = "")
head(data)
View(data)

summary(data)
str(data)

#melihat plot grafik untuk 2 atribut yaitu V4 = panjang kernel dan V5=lebar kernel
qplot(V4, V5, data = data, color=V8)

#mengecek missing value
anyNA(data)

#membagi data ke dalam training dan testing data
benih <- names(data[,1:7])
d<- paste(benih, collapse='+')
d<- paste('V8~', d)
d<- as.formula(d)

n<- round(nrow(data)*0.7)
set.seed(123)
sampel = sample(1:nrow(data),n)
train <- data[sampel,]
test <- data[-sampel,]

#Memodelkan data
##1. Linear
model1 <- svm(d,train, kernel="linear", type="C-classification")
model1

##2. Radial
model2 <- svm(d,train, kernel="radial", type="C-classification")
model2

##3. Polynomial
model3 <- svm(d,train, kernel="polynomial", type="C-classification")
model3

#membuat prediksi dari masing-masing model
pred1 <- predict(model1, newdata = test[,1:7])
pred2 <- predict(model2, newdata = test[,1:7])
pred3 <- predict(model3, newdata = test[,1:7])

#Confusion matrix
##Model 1
confusionMatrix(table(pred1, test$V8))
##Model 2
confusionMatrix(table(pred2, test$V8))
##Model 3
confusionMatrix(table(pred3, test$V8))

#Plot dari model terbaik (Model Linear)
plot(model1, data, V4~V5)


modelSVM <- svm(V8~., data=data)
summary(modelSVM)

set.seed(1234)
sampel <- sample(2,nrow(data), replace=T,prob=c(0.8,0.2))
trainingdat <- data[sampel==1, ]
testingdat <- data[sampel==2, ]
print(paste("Jumlah Train Data :", nrow(trainingdat)))
print(paste("Jumlah Test Data :", nrow(testingdat)))

model1 <- svm(data, trainingdat, kernel = "linear", type= "C-classification")
