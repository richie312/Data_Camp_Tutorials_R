## Required Packages

library(dplyr)
library(gbm)
library(corrplot)

## Read the dataset

data<-read.csv("train.csv",stringsAsFactors = FALSE)
test<-read.csv("test.csv",stringsAsFactors = FALSE)
sample<-read.csv("sample_submission.csv",stringsAsFactors = FALSE)

## Examine the dataset

str(data)

## Convert the characters from anonymous variable into numeric

myLetters<-toupper(letters[1:26])
data$var2<-match(data$var2, myLetters)

## Select the variables for computation of correlation matrix

data_cor<-data[,c(3,4,5,6,7,8)]
cor_mat<-cor(data_cor)
corrplot(cor_mat)

## Split the dates into year and month, keeping date column intact

data$datetime<- as.Date(data$datetime)
data$year<- as.numeric(format(data$datetime, format = "%Y"))
data$mon<-as.numeric(format(data$datetime,format = "%m"))

## Format test data file
test$datetime<- as.Date(test$datetime, "%d-%m-%Y")
test$year<- as.numeric(format(test$datetime, format = "%Y"))
test$mon<-as.numeric(format(test$datetime,format = "%m"))


##  Create the function to count the number of rows in dataset as the dataset unfold with changing months and years


Run_count=function(Month,Year){
  ## Dynamic Dataset
  data1=data%>% filter(mon<=Month & year<=Year)
  return(nrow(data1))
}

Run_count(12,2017)
Run_count(6,2016)

#### Build the user defined function for Training Model

Run_pred=function(Month,Year){
  newdata=test%>%select(ID,temperature,var1,pressure,windspeed,year,mon)%>%filter(mon==Month & year== Year)
  
  ## Dynamic Dataset
  data1=data%>% filter(mon<=Month & year<=Year)
  
  gbmM1<-gbm(electricity_consumption~temperature+var1+pressure+windspeed,mon,
             distribution="gaussian",interaction.depth=3, n.cores=detectCores()/2, 
             n.trees = 2500,shrinkage = 0.001, data = data1 )
  gbmTrainPredictions <- predict.gbm(object=gbmM1, newdata=newdata, n.trees=2500, 
                                     type="response")
  sample<-data.frame(newdata$ID,gbmTrainPredictions)
  return(sample)
}


#### User defined function for each month and year to compute the prediction values

##### UDF for 2013 as this has only 5 months
Pred_Year=function(x){y<-NULL;
  for(j in unique(7:12)){t<-Run_pred(j,x)
  y<-rbind(y,t)}
  return(y)
}

Pred_2013<-Pred_Year(2013)

##### General UDF for all the years

Pred_Year=function(x){y<-NULL;
for(j in unique(1:12))
{t<-Run_pred(j,x)
  y<-rbind(y,t)}
return(y)
}

Pred_2014<-Pred_Year(2014)
Pred_2015<-Pred_Year(2015)
Pred_2016<-Pred_Year(2016)
Pred_2017<-Pred_Year(2017)


#### Combine the data in one file

Sample_Submission<-data.frame(rbind(Pred_2013,Pred_2014,Pred_2015,Pred_2016,Pred_2017))

sample_submission<-write.csv(Sample_Submission,"sample_submission1.csv")
  



