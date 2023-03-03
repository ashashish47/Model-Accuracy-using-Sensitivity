getwd()
#Extracting data
data=read.csv("PROB Assignment data.csv")

#applying Logistic Regression on data
lr=glm(data$Match.Result~data$Home.Away+data$Toss,data=data,family=binomial(link="logit"))
s_lr=summary(lr)
s_lr

#Estimated probability
beta0.hat=s_lr$coefficients[1,1]
beta1.hat=s_lr$coefficients[2,1]
beta2.hat=s_lr$coefficients[3,1]
beta0.hat
beta1.hat
beta2.hat
p.hat=predict(lr,type="response")
p.hat


#Estimated Results with threshold=mean of p.hat
thres=mean(p.hat)
thres
Predicted_val=0.21014+(0.92323*data$Home.Away)-(0.08371*data$Toss)
Predicted_val=ifelse(p.hat>thres,1,0)
Predicted_val

#Confusion Matrix
library(caret)
d2=factor(data$Match.Result)
d1=factor(Predicted_val)
confusionMatrix(d1,d2,positive = "1")
table(data$Match.Result,Predicted_val)

