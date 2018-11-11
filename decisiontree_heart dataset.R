Heart<-read.csv("Heart_data.csv")
str(Heart)
Heart_train <- Heart[1:270,]
Heart_test <- Heart[271:360,]
C5.0_model <- C5.0(Heart_train[,1:7], Heart_train[,8])
summary(C5.0_model)
Predicted=predict(C5.0_model,Heart_test[,1:7])
confusionMatrix(Predicted,Heart_test[,8])
Predicted_prob=predict(C5.0_model,Heart_test[,1:7],type="prob");Predicted_prob
mult_measures=prediction(Predicted_prob[,2], Heart_test[,8])
ROC=performance(mult_measures,measure="tpr",x.measure="fpr")                         
plot(ROC)
plot(C5.0_model)
plot(C5.0_model, subtree =2)
C5.0_model_boost=C5.0(Heart_train[,1:7], Heart_train[,8],trials=20)
summary(C5.0_model_boost)                      

ROC2<-performance(mult_measures,measure="auc")
auc=as.numeric(ROC2@y.values)
auc_lgnd=paste(c("AUC",auc),collapse="")
plot(ROC,col="red")
abline(a=0,b=1)
legend(.6,.3,auc_lgnd,lty=1,lwd=1,col="red")
