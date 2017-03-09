#library(ROCR)
cancer<-read.table("pros.csv",header=T,sep=",",na.strings=c(""))
#remove missing VOL
cancer$VOL[cancer$VOL==0]<-NA
cancer<- cancer[complete.cases(cancer),]
#remove ID column
cancer <- cancer[, !colnames(cancer) %in% c('ID')]
#split dataset
set.seed(1)
train_ind<- sample(1:nrow(cancer),2/3*nrow(cancer))
train<-cancer[train_ind,]
test<-cancer[-train_ind,]
test_predictors<-cancer[-train_ind,-cancer$CAPSULE]
test_truth<-cancer$CAPSULE[-train_ind]
#fit logistic regression model
cancerGLM<-glm(formula=CAPSULE~.,family=binomial,data=train)
summary(cancerGLM)
#make prediction
prediction<-predict(cancerGLM,newdata=test_predictors,type='response')
prediction<-as.numeric(prediction>=0.5)
table(test_truth,prediction)
#FPR & TPR
missclassified<-prediction[prediction!=test_truth]
FPandFN<-NROW(missclassified)
FP<-NROW(missclassified[missclassified==1])
FN<-FPandFN-FP
correctclass<-prediction[prediction==test_truth]
TPandTN<-NROW(correctclass)
TP<-NROW(correctclass[correctclass==1])
TN<-TPandTN-TP
FPR_0.5<-FP/(FP+TN)
TPR_0.5<-TP/(TP+FN)
#ROC
p0<-c(0:10)
FPR<-rep(0,11)
TPR<-rep(0,11)
for(i in (1:11)){
  pred<-predict(cancerGLM,newdata=test_predictors,type='response')
  pred<-as.numeric(pred>=p0[i]/10)
  false<-pred[pred!=test_truth]
  FPandFN<-NROW(false)
  FP<-NROW(false[false==1])
  FN<-FPandFN-FP
  correct<-pred[pred==test_truth]
  TPandTN<-NROW(correct)
  TP<-NROW(correct[correct==1])
  TN<-TPandTN-TP
  FPR[i]<-FP/(FP+TN)
  TPR[i]<-TP/(TP+FN)
}
plot(FPR,TPR,main="ROC", xlab="FPR",ylab="TPR",type="b")
abline(a=0,b=1)
#cancer.pred <- prediction(prediction,test_truth)
#cancer.perf <- performance(cancer.pred,measure="tpr",x.measure="fpr")
#plot(cancer.perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
#abline(a=0,b=1)

