## Set the working directory
setwd("D:/Documents/Hackathon/McKinsey")

## Required Packages

library(corrplot)
library(gbm)
library(caTools)

## Read the train and testdataset

master_data<-read.csv("train.csv")
test<-read.csv("test.csv")
sample<-read.csv("Sample_Solution.csv")
names(master_data)

### Subset Data for Logistic Regression Model

data_log<-master_data[,c(1,2,6,8,9,10,12,15,16,17,18,19,20,21,22)]
str(data_log)

## Convert all of them to numeric
data_log<-sapply(data_log, as.numeric)
data_log<-as.data.frame(data_log)

## Examine the data Structure
str(data_log)

## Correlation matrix
data_log_cor<-cor(data_log_cor, use='complete.obs')
cor_matrix<-corrplot(data_log_cor)

## Missing Values Computation
is.na(data_log)

data_log$Loan_Amount[is.na(data_log$Loan_Amount)] <- mean(data_log$Loan_Amount,na.rm=T)
data_log$Interest_Rate[is.na(data_log$Interest_Rate)] <- mean(data_log$Interest_Rate,na.rm=T)
data_log$EMI[is.na(data_log$EMI)]<-mean(data_log$EMI,na.rm=T)
data_log$Existing_EMI[is.na(data_log$Existing_EMI)]<-mean(data_log$Existing_EMI,na.rm=T)

## Replace the NA values for categorical variable by the maximum frequency of the factor
data_log$City_Category[is.na(data_log$City_Category)] <- which.max(tabulate(data_log$City_Category))
data_log$Employer_Category1[is.na(data_log$Employer_Category1)] <- which.max(tabulate(data_log$Employer_Category1))
data_log$Employer_Category2[is.na(data_log$Employer_Category2)] <- which.max(tabulate(data_log$Employer_Category2))
data_log$Loan_Period[is.na(data_log$Loan_Period)]<-which.max(tabulate(data_log$Loan_Period))


## Check if there any existence of na value in Test Dataset

any(is.na(data_log))

## Test Dataset and Replace NA values by mean
Test_Data=test[,c(1,2,6,9,10,12,15,16,18,19,21)]

Test_Data<-sapply(Test_Data, as.numeric)
Test_Data<-as.data.frame(Test_Data)

is.na(Test_Data)
Test_Data$Interest_Rate[is.na(Test_Data$Interest_Rate)] <- mean(Test_Data$Interest_Rate,na.rm=T)
Test_Data$Existing_EMI[is.na(Test_Data$Existing_EMI)]<-mean(Test_Data$Existing_EMI,na.rm=T)

## Replace the NA values for categorical variable by the maximum frequency of the factor

Test_Data$Loan_Period[is.na(Test_Data$Loan_Period)] <- which.max(Test_Data$Loan_Period)
Test_Data$City_Category[is.na(Test_Data$City_Category)] <- which.max(tabulate(Test_Data$City_Category))
Test_Data$Source_Category[is.na(Test_Data$Source_Category)] <- which.max(tabulate(Test_Data$Source_Category))
Test_Data$Employer_Category2[is.na(Test_Data$Employer_Category2)] <- which.max(tabulate(Test_Data$Employer_Category2))

## Check if there any existence of na value in Test Dataset

any(is.na(Test_Data))


## Alternate_Model

M=gbm(data_log$Approved~Gender+City_Category+Employer_Category2+Primary_Bank_Type+Monthly_Income+Source_Category
       +Existing_EMI+Loan_Period+Interest_Rate+Var1,distribution='bernoulli',n.trees=3000,data=data_log)


Pred=predict.gbm(M,test, n.trees=3000, type='response')


### Write.csv file

Sample=data.frame(test$ID,Pred)
colnames(Sample)=c("ID","Approved")
write.csv(Sample,"Sample_Solution.csv")

