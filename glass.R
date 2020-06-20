library(readr)
glass<-read.csv("E:\\Assignment\\knn\\glass.csv")
class(glass)
View(glass)
str(glass)
dim(glass)
class(glass)
table(glass$Type)
class(glass$Type)## integer

sum(is.na(glass))
## converting dependent variable in to factor.
glass$Type<-factor(glass$Type)
glass$Type
class(glass$Type)
# table or proportation of enteries in the datasets.
round(prop.table(table(glass$Type))*100,digits = 1)
#Create a function to normalize the data
norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
#norm(c(1,2,3,4,5))
#norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_n<-as.data.frame(lapply(glass[1:9],norm))
View(glass_n)
#create training and test datasets
glass_train<-glass_n[1:150,]
View(glass_train)
glass_test<-glass_n[151:214,]
glass_test
#Get labels for training and test datasets

glass_train_labels<-glass[1:150,10]
glass_train_labels
glass_test_labels<-glass[151:214,10]
glass_test_labels
# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc1<-NULL
train_acc1<-NULL
for (i in seq(3,100,2)) {
  train_glass_pred<-knn(train = glass_train,test = glass_train,cl=glass_train_labels,k=i)
  train_acc1<-c(train_acc1,mean(train_glass_pred==glass_train_labels))
  test_glass_pred<-knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=i)
  test_acc1<-c(test_acc1,mean(test_glass_pred==glass_test_labels))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,100,2),train_acc1,type="l",main="Train_accuracy",col="blue")
plot(seq(3,100,2),test_acc1,type="l",main="Test_accuracy",col="red")

acc_neigh_df1<- data.frame(list(train_acc1=train_acc1,test_acc1=test_acc1,neigh=seq(3,100,2)))
acc_neigh_df1
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df1,aes(x=neigh))+
  geom_line(aes(y=train_acc1,colour="train_acc1"),lwd=1.5)+
  geom_line(aes(y=test_acc1,colour="test_acc1"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc1","test_acc1"),values = c("train_acc1"="blue","test_acc1"="red"))
### Plotting 2 different graphs on same co-ordinate axis
glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
glass_test_pred
class(glass_test_pred)
View(glass_test_pred)
table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)
install.packages("gmodels")
library("gmodels")
# cross table
CrossTable(x = glass_test_labels, y =glass_test_pred,
           prop.chisq=FALSE)
library(caret)
library(e1071)
confusionMatrix(table(glass_test_pred,glass_test_labels)) 
class(glass_test_labels) # factor
class(test_glass_pred)

