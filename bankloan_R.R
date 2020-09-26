#remove objects from RAM
rm(list=ls())

#set working directory
setwd("C:/Users/HP/Desktop/Bank Project")

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)
rm(x)

########### load dataset ###############

bankdata = read.csv("bank-loan.csv")

############## Understand Data ################

class(bankdata)

str(bankdata)

dim(bankdata)

#change datatype as per data

bankdata$age = as.numeric(bankdata$age)
bankdata$ed = as.factor(bankdata$ed)
bankdata$employ = as.numeric(bankdata$employ)
bankdata$address=as.numeric(bankdata$address)
bankdata$income = as.numeric(bankdata$income)

str(bankdata)

##################### understand summary of all variables ###############

summary(bankdata)

##################### missing value anlysis and imputaion #####################

sum(is.na(bankdata))
summary(is.na(bankdata))

#calculated and imputed NA by taking mode of default column 

bankdata[is.na(bankdata)] <- 0

sum(is.na(bankdata))

############################# outlier analysis ###########################

boxplot(bankdata[,c('age','employ','address','income','debtinc','creddebt','othdebt')])
        
########## outlier detection and removal  ##########

for (x in c('age','employ','address','income','debtinc','creddebt','othdebt'))
{
  value = bankdata[,x][bankdata[,x] %in% boxplot.stats(bankdata[,x])$out]
  bankdata[,x][bankdata[,x] %in% value] = NA
} 

library(tidyr)
bankdata = drop_na(bankdata)
as.data.frame(colSums(is.na(bankdata)))

dim(bankdata)

######################### feature selection #####################

numeric_col=c('age','employ','address','income','debtinc','creddebt','othdebt')

library(corrgram)

corrgram(bankdata[,numeric_col],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis Plot")


######################### Feature scaling ###################################

##### histogram 

hist(bankdata$age)
hist(bankdata$employ)
hist(bankdata$address)  
hist(bankdata$income)
hist(bankdata$debtinc)
hist(bankdata$creddebt)
hist(bankdata$othdebt)

##### skeweness test 

library(e1071)

for(x in numeric_col)
{
  print(x)
  skewtest = skewness(bankdata[,x])
  print(skewtest)
}

 

################### sampling of data as training and testing ###################

set.seed(1234)

train.index = createDataPartition(bankdata$default, p = .80, list = FALSE)
train = bankdata[ train.index,]
test  = bankdata[-train.index,]

train$default=as.factor(train$default)
str(train$default)

############### Logistic Regression ###############

library(caret)
logit_model = glm(default ~ ., data = train, family = "binomial")
summary(logit_model)

#prob_pred = predict(logit_model, test)
#logit_predict = predict(logit_model , test[] ,type = 'response' )
#logit_predict <- ifelse(logit_predict > 0.5,1,0) # Probability check
#CM= table(test , logit_predict)



##Decision tree for classification
#Develop Model on training data
library(C50)
str(bankdata$default)
bankdata$default[bankdata$default %in% "1"] = "yes"
bankdata$default[bankdata$default %in% "0"] = "no"

C50_model = C5.0(default ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)


#Lets predict for test cases
C50_Predictions = predict(C50_model, test, type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$default, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
#FNR = FN/FN+TP 


################## Random Forest ##################

RF_model = randomForest(default ~ ., train, importance = TRUE, ntree = 500)
#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$default, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
#False Negative rate
#FNR = FN/FN+TP 

########################## Here We got more accuracy with Random Forest ########
Result = data.frame('Actual_target' = test$default, 'Predicted_target' =RF_Predictions  )
write.csv(Result,"PREDICTION_R.csv",row.names=FALSE)

