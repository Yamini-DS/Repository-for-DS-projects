rm(list=ls())
#checking the working directory
getwd() 
#loading the libraries
L = c('tidyr','ggplot2','corrgram','usdm','caret','lattice','DMwR','rpart','randomForest')
#loading packages
lapply(L, require, character.only = TRUE)
rm(L)
#loading the train and test datasets
train_df=read.csv('Sa_mission(train).csv',header = TRUE)
test_df=read.csv('Sa_mission(test).csv',header = TRUE)
#checking dimensions
dim(train_df)
dim(test_df)
#Structure of data
str(train_df)
str(test_df)
#Summary of Data
summary(train_df)
summary(test_df)
head(train_df)
#checking for missing values
sum(is.na(train_df))
sum(is.na(test_df))
#storing numeric data without ID_code 
train_data=train_df[,2:202]
View(train_data)
test_data=test_df[,2:201]
View(test_data)
dim(train_df)
dim(test_df)
#factorize the target variable
train_data$target = factor(train_data$target, levels = c(0, 1))
#column names
cnames=colnames(train_df[,3:202])
cnames
#Plot graph for o & 1s in target variable
target_df <- data.frame(table(train_df$target))
colnames(target_df) <- c("target", "frequency")
ggplot(data=target_df, aes(x=target, y=frequency, fill=target)) +
  geom_bar(position = 'dodge', stat='identity', alpha=0.5) +theme_classic()
#ploting target distribution in var_0
ggplot(data=train_data, aes_string(x=train_data$var_0,y=train_data$target))+
  geom_point(color="blue",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             size=3,
             stroke = 2)+labs(x='var_0',y='target')
#Distribution of data in variables
qqnorm(train_data$var_0)
hist(train_data$var_0)
qqnorm(test_data$var_0)
hist(test_data$var_0)
#Removing outliers from the train set
for (i in cnames) {
  val = train_data[,i][train_data[,i]%in% boxplot.stats(train_data[,i])$out]
  print(length(val))
  train_data[,i][train_data[,i] %in% val] = NA
}
#Again checking for missing data after outliers
apply(train_data, 2,function(x) {sum(is.na(x))})
train_data= drop_na(train_data)
dim(train_data)
#copying data
data_no_outliers =train_data
dim(train_data)
#Removing outliers from the test set
for (i in cnames) {
  val = test_data[,i][test_data[,i]%in% boxplot.stats(test_data[,i])$out]
  print(length(val))
  test_data[,i][test_data[,i] %in% val] = NA
}
#Again checking for missing data after outliers on test data
apply(test_data, 2,function(x) {sum(is.na(x))})
test_data= drop_na(test_data)
#copying data
test_no_outliers =test_data
dim(test_data)
dim(train_data)
#Correlation
correlationMatrix <-cor(train_data[,2:201])
print(correlationMatrix)
highlyCorrelated <-findCorrelation(correlationMatrix,cutoff = 0.75)
print(highlyCorrelated)
###So there is no high correlation in the variables 
      #so keeping all the 199 variables as it is without removing any
#normalization on train_data
for(i in cnames){
  print(i)
  train_data[,i] = (train_data[,i] - min(train_data[,i]))/
    (max(train_data[,i] - min(train_data[,i])))
}
#View(train_data)
#normalization on test_data
for(i in cnames){
  print(i)
  test_data[,i] = (test_data[,i] - min(test_data[,i]))/
    (max(test_data[,i] - min(test_data[,i])))
}
#View(test_data)
#install.packages("ROSE")
library(ROSE)
#check table
table(train_data$target)
#check classes distribution
prop.table(table(train_data$target))
#balancing the data using ROSE technique
train_rose <- ROSE(target ~ .,data=train_data,p=0.5,N=175107,seed=1)$data
table(train_rose$target)
prop.table(table(train_rose$target))
#library(caret)
#Dividing data into to train and test
set.seed(272)
train_index = createDataPartition(train_rose$target,p=0.7,list = FALSE)
train = train_rose[train_index,]
test = train_rose[-train_index,]
dim(train)
dim(test)
#plot after splitting the data into train and test
train1 <- data.frame(table(train$target))
train1
colnames(train1) <- c("target", "frequency")
ggplot(data=train1, aes(x=target, y=frequency, fill=target)) +
  geom_bar(position = 'dodge', stat='identity', alpha=0.5) +theme_classic()
#Fitting logistic regression model
logit_model = glm(formula = target ~ .,family = binomial,data = train)
summary(logit_model)
# Predicting the Test set results
log_pred = predict(logit_model, type = 'response', newdata = test)
y_pred = ifelse(log_pred > 0.5, 1, 0)
y_pred
Confmatrix_log = table(test$target, y_pred)
confusionMatrix(Confmatrix_log)
#The accuracy score on using logistic algorithm is 71.2%,kappa 0.4239,
   # Sensitivity is 0.7094,Specificity is 0.7146 and other statistic
       #results obtained from logistic model
library(caTools)
library(e1071)
dim(test)
str(test)
test[,1]
test[,-1]]
dim(test[,-1])
#Naive Bayes model
model_nb = naiveBayes(target ~ ., data =train)
summary(model_nb)
#predict on test cases 
pred_nb = predict(model_nb, test[,2:201], type = "class")
pred_nb
#Look at confusion matrix
Confmatrix_nb = table(test[,1],pred_nb)
confusionMatrix(Confmatrix_nb)
#The accuracy score on using Naive Bayes on Santander mission customer
  #dataset is 76.27%,kappa score is 0.5254,Sensitivity is 0.7560 and 
   #specificity is 0.7698 and many other statistics are obtained
#Random Forest
#install.packages("randomForest")
library(randomForest)
library(ggplot2)
#library(inTrees)
rf_model = randomForest(target ~ ., train, importance = TRUE, ntree = 100)
summary(rf_model)
#predict on test data
rf_pred = predict(rf_model, test[,-1])
rf_pred
Confmatrix_rf = table(test$target, rf_pred)
confusionMatrix(Confmatrix_rf)
#The accuracy score on using Random Forest is 69.66% which is almost 
 #70% then when compared to other models this not the model to select
  #as the even the FP,FN values are also high(from confusion matrix interpretations)
   #and all other statistics can be seen in the results section
#From all the models Logistic,NB,RF the score seems to be 71%,76% and 70%
#So from all these results it can be said that Naive Bayes is working well
  #from all the other built models. So make final predictions from the same model
#Predict the test_dataset outcome
head(test_data)
dim(test_data)
final_test_pred = predict(model_nb, test_data[,1:200], type = 'class')
final_test_pred
#Saving output to file
#swrite.csv(final_test_pred,file = 'C:/Users/Click/Desktop/Bike rental/Finalclassification_R.csv',row.names = F) 



