#Make sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA.
Titanic<-read.table("titanic.csv",header=T,sep=",",na.strings=c(""))

#choose features that we are interested in
titanic_2<-Titanic[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")]

#check missing data
#sapply(titanic_2,function(x) sum(is.na(x)))
#handle missing data
#for"Age",replace missing data with mean
#m=mean(titanic_2$Age,na.rm=TRUE)
#titanic_2$Age[is.na(titanic_2$Age)] <- m
#for"Embarked",discard the 2 rows
#titanic_2 <- titanic_2[!is.na(titanic_2$Embarked),]

#remove instances missing age information
titanic_2<- titanic_2[complete.cases(titanic_2), ]

#make sure all categorical predictors are read in factor type
#titanic_2$Pclass<-as.factor(titanic_2$Pclass)

sapply(titanic_2, class)

#split dataset into training and test sets
#pseudo-random number generation(can be reproduced by set.seed(1))
set.seed(1)
train_ind<- sample(1:nrow(titanic_2),2/3*nrow(titanic_2))
train<-titanic_2[train_ind,]
test<-titanic_2[-train_ind,]
test_predictors<-titanic_2[-train_ind,-titanic_2$Survived]
test_truth<-titanic_2$Survived[-train_ind]

#fit logistic regression model in training data
#In this post we call the model ¡°binomial logistic regression¡±,
#since the variable to predict is binary, 
#however, logistic regression can also be used to predict a 
#dependent variable which can assume more than 2 values. 
titanicGLM<-glm(formula=Survived~.,family=binomial,data=train)
summary(titanicGLM)

#fit model in test set
prediction<-predict(titanicGLM,newdata=test_predictors,type='response')
prediction<-as.numeric(prediction>=0.5)
table(test_truth,prediction)

#compute accuracy
#method 1
TP<-73
TN<-118
FP<-20
FN<-27
acc1<-(TP+TN)/(TP+TN+FP+FN)

#method 2
misClasificError <- mean(prediction!=test_truth)
acc2<-1-misClasificError

#unbiased accuracy
acc3<-0.5*TP/(TP+FN)+0.5*TN/(TN+FP)