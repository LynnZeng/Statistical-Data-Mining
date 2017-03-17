corollas<-read.table("ToyotaCorolla.csv",header = TRUE,sep=",")

#Delete unuseful columns
corollas<-corollas[, colnames(corollas) %in% c('Price','Age_08_04', 'KM')]

#Check and handle missing data
sapply(corollas,function(x) sum(is.na(x)))
#If there is missing data, delete the whole row
corollas<- corollas[complete.cases(corollas), ]

#check classes of variables 
sapply(corollas, class)
#Pick 1/2 observations to be training set
set.seed(1)
train_id=sample(nrow(corollas),nrow(corollas)/2)

#Cross Validation
#Fit a linear regression model
lm.fit=lm(Price~Age_08_04+KM,data=corollas,subset=train_id)
summary(lm.fit)
#Calculate MSE of Validation set
validation<-corollas[-train_id,]
pred<-predict(lm.fit,validation)
mse=mean((pred-validation$Price)^2)

#Try quadratic and cubic terms
lm.fit2 = lm(Price~poly(Age_08_04, 2)+poly(KM, 2),
             data=corollas, subset=train_id)
lm.fit3 = lm(Price~poly(Age_08_04, 3)+poly(KM, 3),
               data=corollas, subset=train_id)
pred2<-predict(lm.fit2,validation)
mse2=mean((pred2-validation$Price)^2)
pred3<-predict(lm.fit3,validation)
mse3=mean((pred3-validation$Price)^2)


#############LOOCV#################
library(boot)
cv.error=rep(0,5)

for(i in (1:5)){
  glm.fit<-glm(Price~poly(Age_08_04,i)+poly(KM,i),data=corollas)
  cv.error[i]<-cv.glm(corollas,glm.fit)$delta[1]
}
 
cv.error
plot(cv.error, type="b")

#cubic model
glm.fit3 = glm(Price~poly(Age_08_04, 3)+poly(KM,3),data= corollas)
summary(glm.fit3)

##################### K Fold Cross-Validation#################
cv.error=rep(0,5)
for (i in (1:5)) {
  glm.fit = glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
  cv.error[i] = cv.glm(corollas, glm.fit, K=10)$delta[1]
  }
 cv.error
plot(cv.error, type="b")

#########################Take Home Question Part2#################
#create a synthetic data set DF and make a plot
n = 1000
x1 = runif(n)
x2 = runif(n, -2, 1)
z = (x1-0.2)*(x1-0.5)*(x1-0.9) * 25 - x2*(x2+1.2)*(x2-0.8) + rnorm(n)/3
y = as.integer(z>0)
plot(x1, x2, col=c("red", "blue")[y+1])
DF = data.frame(x1,x2,y)

#fit logistic regression model
#LOOCV
cv.error=rep(0,5)
for (i in (1:5)) {
  glm.fit = glm(y~poly(x1, i)+poly(x2,i), data = DF,family=binomial(link='logit'))
  cv.error[i] = cv.glm(DF, glm.fit)$delta[1]
}
cv.error
plot(cv.error, main="LOOCV_Logistic Regression",type="b")

#K Fold CV
cv.error=rep(0,5)
for (i in (1:10)) {
  glm.fit = glm(y~poly(x1, i)+poly(x2,i), data = DF,family=binomial(link='logit'))
  cv.error[i] = cv.glm(DF, glm.fit,K=10)$delta[1]
}
cv.error
plot(cv.error, main="KFoldCV_Logistic Regression",type="b")



