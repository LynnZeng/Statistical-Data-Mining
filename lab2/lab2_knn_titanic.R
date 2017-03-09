library(class)
#Make sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA.
Titanic<-read.table("titanic.csv",header=T,sep=",",na.strings=c(""))

#choose features that we are interested in
titanic_2<-Titanic[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")]

#remove instances missing age information
titanic_2<- titanic_2[complete.cases(titanic_2), ]

#check for factor variable and convert them into numeric
sapply(titanic_2, class)

titanic_2$Sex <- model.matrix( ~ Sex - 1, data=titanic_2 )
titanic_2$Embarked <- model.matrix( ~ Embarked - 1, data=titanic_2 )

#titanic_2$Sex <- as.numeric(titanic_2$Sex )
#titanic_2$Embarked <- as.numeric(titanic_2$Embarked)
#cannot covert factor variables to dummy/binary variables


#We start with a formula with the tilde symbol ~. 
#This means that we want to model the observations 
#using the variables to the right of the tilde. 
#Then we put the name of a variable, which tells us which samples are in which group.

#split dataset into training and test sets
#pseudo-random number generation(can be reproduced by set.seed(1))
set.seed(1)
train_ind<- sample(1:nrow(titanic_2),2/3*nrow(titanic_2))

#Normalization
titanic_normal <- scale(titanic_2[,!names(titanic_2) %in% 'Survived'])
titanic.train <- titanic_normal[train_ind, ]
titanic.test <- titanic_normal[-train_ind, ]

#Store the outcome colomn seperately 
train.survive=titanic_2$Survived[train_ind]
test.survive=titanic_2$Survived[-train_ind]

#Implement the KNN algorithm,k=1
knn.pred=knn(titanic.train,titanic.test,train.survive,k=3)
table(knn.pred,test.survive)
mean(knn.pred==test.survive)

#Implement the KNN algorithm,k=1,3,5,10,20,50
krange<-c(1,3,5,10,20,50)

error<-rep(0,6)

for (i in (1:6)){
  set.seed(1);
  knn.pred=knn(titanic.train,titanic.test,train.survive,k=krange[i])
  error[i]<-1-mean(knn.pred==test.survive)
  print(error[i])
 
}

