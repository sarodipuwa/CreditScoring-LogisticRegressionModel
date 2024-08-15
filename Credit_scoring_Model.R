#is.na(I$bmi)
#I$bmi[is.na(I$bmi)] <- mean(I$bmi,na.rm=TRUE)

### Deleting the value
#I <- na.omit(I)
P <- read.csv("creditscoringData.csv",stringsAsFactors = TRUE)
View(P)
summary(P)
is.na(P$marital)
P$marital[is.na(P$marital)] <- mean(P$marital,na.rm=TRUE)

### Deleting the value
P<- na.omit(P)
library(fastDummies)
#P = dummy_cols(P,select_columns = c("job"))#
#View(P)
P <- dummy_cols(P, select_columns = c("marital","education","poutcome"))
View(P)
P$housing <- ifelse(P$housing == "yes",1,0)
P$loan<- ifelse(P$loan == "yes",1,0)
P$default<- ifelse(P$default == "yes",1,0)
P$y<- ifelse(P$y == "yes",1,0)
library(corrplot) #For correlation
library(caTools) #For Splitting the data
library(psych) # For Pair.panel chart
library(HH) ## for mac write library(car) 
library(car)
cr <- cor(P[sapply(P,is.numeric)])
corrplot(cr, type = "full")
corrplot(cr, method="number")
write.csv(cr,"P_corr.csv")
P$y <- as.factor(P$y)
split <- sample.split(P$y, SplitRatio = 0.7)
train<-subset(P, split=="TRUE")
test<-subset(P, split=="FALSE")
library(lmtest)
model1 <- glm(y~duration,data = train,family = binomial)
summary(model1) 
##AIC and residual deviance should be decreasing also residual deviance should be less than null deviance 
##residual deviance tells us the realationhsip between depenedent and independent variables but null deviance captures only intercept
model2 <- glm(y~duration+poutcome_success,data = train,family = binomial)
summary(model2)  
vif(model2)
##vif should be less than 5 to avoid problem of multicollinerity
model3 <- glm(y~duration+poutcome_success+previous,data = train,family = binomial)
summary(model3)  
vif(model3)

model4 <- glm(y~duration+poutcome_success+previous+education_tertiary,data = train,family = binomial)
summary(model4)  
vif(model4)

model5 <- glm(y~duration+poutcome_success+previous+education_tertiary+poutcome_other,data = train,family = binomial)
summary(model5)  
vif(model5)

model6 <- glm(y~duration+poutcome_success+previous+education_tertiary+poutcome_other+age,data = train,family = binomial)
summary(model6)  
vif(model6)

model7 <- glm(y~duration+poutcome_success+education_tertiary+poutcome_other+marital_divorced+age,data = train,family = binomial)
summary(model7)  
vif(model7)

model8 <- glm(y~duration+poutcome_success+education_tertiary+age+poutcome_other+marital_divorced+poutcome_failure,data = train,family = binomial)
summary(model8)  
vif(model8)
model9 <- glm(y~duration+poutcome_success+education_tertiary+poutcome_other+marital_divorced+poutcome_failure+default,data = train,family = binomial)
summary(model9)  
vif(model9)

# Check VIF
vif_values <- vif(glm(y~duration+poutcome_success+age+education_tertiary+poutcome_other+marital_divorced+poutcome_failure,data = train,family = binomial))
print(vif_values)
###PREDICTION
residual<-predict(model8,test,type="response")
head(residual)
head(test$y)
##how will you decide 0.4 --> apply the formula for specificity and see where you get the highest 
table(Actualvalue=test$y,Predicted = residual>0.4)
library(ROCR)

ROCRpredicted<-prediction(residual,test$y)
ROCRperformance<-performance(ROCRpredicted,"tpr","fpr")
plot(ROCRperformance, main = "ROC Curve", col = "blue")
plot(ROCRperformance,colorsize=TRUE,print.cutoffs.at=seq(0.2,by=0.1))

