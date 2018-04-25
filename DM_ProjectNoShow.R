# Data Mining 1: Final Project
# Title: Project No Show
# Group Members: 
#     Gregh Rhodes
#     Justin Scarborough
# This project is due on 4/26/2018

# Use this file to run the main program. Each group member
# will create separate files to perform various data managemnet
# tasks, but this file will be used to run our blocks of files

## Main ##
# Load libraries 
  #install.packages("tidyverse") # alternate way to get dplyr
  library(dplyr)  # include in case we want to convert to tbl. See DataCamp Tutorial
  library(readr)  # include to read CSV files
  library(ggplot2)# include for nice visuals


# Import Data
  NoShowData <- read_csv("KaggleV2-May-2016.csv")
  # Example for reading one column
  # SMSData <-read.csv("KaggleV2-May-2016.csv")[ ,c('SMS_received')] for reading one column

  
# Initial Data Exploration
  names(NoShowData)
  str(NoShowData)
  dim(NoShowData)

  
# Data Cleaning
  #Rename columns to more convenient variables
  names(NoShowData) <- list("PatientId","AppointmentId","Gender","ScheduledDay","AppointmentDay","Age","Neighborhood","Scholarship","Hypertension","Diabetes","Alcoholism","Handicap","SmsReceived","NoShow")
  
  # check for missing data
  sapply(NoShowData, function(x) sum(is.na(x)))
  
  # check for number of unique values in each column
  sapply(NoShowData, function(x) length(unique(x)))

  # 81 unique neighborhoods. let's factorize it,
  # along with Gender (F/M) and NoShow (No=1  Yes=2)
  NoShowData$Neighborhood <- as.factor(NoShowData$Neighborhood)
  NoShowData$Gender <- as.factor(NoShowData$Gender)
  NoShowData$NoShow <- as.factor(NoShowData$NoShow)
  
  # Factorize characteristics into yes/no
  NoShowData$Scholarship <- as.factor(NoShowData$Scholarship)
  NoShowData$Hypertension <- as.factor(NoShowData$Hypertension)
  NoShowData$Diabetes <- as.factor(NoShowData$Diabetes)
  NoShowData$Alcoholism <- as.factor(NoShowData$Alcoholism)
  NoShowData$Handicap <- as.factor(NoShowData$Handicap)
  NoShowData$SmsReceived <- as.factor(NoShowData$SmsReceived)
  
  # Appointment ID is not helpful, remove column
  NoShowData$AppointmentId <- NULL
  
  # Check date ranges
  range(NoShowData$ScheduledDay)
  range(NoShowData$AppointmentDay)
  # Create a new column with the difference between the day scheduled and the appointment day
  NoShowData['DaysScheduledAhead'] <- as.integer(difftime(as.POSIXct(NoShowData$AppointmentDay), trunc.POSIXt(as.POSIXct(NoShowData$ScheduledDay), units='days'), units='days'))
  
  # Create a new column for Appointment day of the week as an integer: 0=Sun, 1=Mon... 6=Sat
  NoShowData['AptWDay'] <- as.factor(as.POSIXlt(NoShowData$AppointmentDay)$wday) #returns Sun - Sat as 0-6

  # Drop these two columns because we don't need them anymore
  # NoShowData$ScheduledDay <- NULL
  # NoShowData$AppointmentDay <- NULL
  
  # Check age range to see if it makes sense
  range(NoShowData$Age)
  NoShowData[NoShowData$Age < 0, ] #select rows with bad data
  ## ERROR: At least one age is Negative    
  ##        Exclude that row.  Example row removal: d<-d[!(d$A=="B" & d$E==0),]
  NoShowData <- NoShowData[NoShowData$Age >= 0, ]
  
  # Check Days Scheduled Ahead to see if the values make sense
  table(NoShowData$DaysScheduledAhead[NoShowData$DaysScheduledAhead < 0])
  ## NOTE: We should consider excluding same-day appointments, ~38k with zero days 
  ##       advance notice.
  
  NoShowData[NoShowData$DaysScheduledAhead < 0, ] #select rows with bad data
  ## NOTE: selecting < 0 was including zero in the results because it's a doulbe value
  ## ERROR: Some Appointments appear to be scheduled AFTER the appoinment has occurred!  
  ##        Dropping the time in ScheduledDay does not cause the error.
  ##        Exclude these any appointment scheduled after the appointment occurred.
  NoShowData <- NoShowData[NoShowData$DaysScheduledAhead >= 0, ]

  
# visualize the data
  #Create to Data Frame using dplyr
  NoShowData_df <- tbl_df(NoShowData)
  
  # See the counts of our categorical variables
  dataCounts = list()
  dataCounts$Gender <- table(NoShowData$Gender)
  dataCounts$Scholarship <- table(NoShowData$Scholarship)
  dataCounts$Hypertension <- table(NoShowData$Hypertension)
  dataCounts$Diabetes <- table(NoShowData$Diabetes)
  dataCounts$Alcoholism <- table(NoShowData$Alcoholism)
  dataCounts$Handicap <- table(NoShowData$Handicap)
  dataCounts$SmsReceived <- table(NoShowData$SmsReceived)
  dataCounts$AptWDay <- table(NoShowData$AptWDay)
  dataCounts                                
  
  # Number of appointments by neighborhood
  #table(NoShowData$NoShow, NoShowData$Neighborhood)
  barplot(table(NoShowData$NoShow, NoShowData$Neighborhood), 
          ylim=c(0,1000+max(table(NoShowData$Neighborhood))),
          las=2,
          cex.names=0.4,
          space = .8,
          col=c("grey","red"),
          ylab="# of Appointments") #las = rotation of x labels, cex = fontsize, space = dist btw cols
  legend("topright", 
         fill =c("grey","red"), 
         legend = c("NoShow=No","NoShow=Yes"))
  
  # Number of appointments per patient
  aptsPerPatient = table(NoShowData$NoShow, NoShowData$PatientId)
  barplot(table(NoShowData$NoShow, NoShowData$PatientId), 
          ylim=c(0,10+max(table(NoShowData$PatientId))),
          las=2,
          xlab="Patient ID", 
          ylab="# of Appointments",
          main="# of Appointments per patient")  
  # legend("topright", 
  #        fill =c("grey","red"), 
  #        legend = c("NoShow=No","NoShow=Yes"))
  
  # Bar Plots ######################
  # Compare NoShow = Yes vs. Noshow = No
  plot(NoShowData$Gender, NoShowData$NoShow, xlab='Gender', ylab='No Show?', main='No Shows Based on Gender')
  
  ## JUSTIN: These plots are not very effictive.  Maybe a histogram or boxplot of noshows by Days Scheduled Ahead is better
  plot(NoShowData$DaysScheduledAhead, NoShowData$NoShow, yaxt='n', ylim=c(0.5,2.5), xlab='Days Scheduled Ahead', ylab='No Show?', main='No Shows Based on Advanced Schedule Days')
  axis(2, at=c(1,2), labels=c('No','Yes'))
  plot(NoShowData$Age, NoShowData$NoShow, yaxt='n', ylim=c(0.5,2.5), xlab='Patient Age', ylab='No Show?', main='No Shows Based on Age')
  axis(2, at=c(1,2), labels=c('No','Yes'))

  plot(NoShowData$Scholarship, NoShowData$NoShow, xlab='Scholarship?', ylab='No Show?', main='No Shows Based on Scholarship')
  plot(NoShowData$Hypertension, NoShowData$NoShow, xlab='Hypertension?', ylab='No Show?', main='No Shows Based on Hypertension')
  plot(NoShowData$Diabetes, NoShowData$NoShow, xlab='Diabetes?', ylab='No Show?', main='No Shows Based on Diabetes')
  plot(NoShowData$Alcoholism, NoShowData$NoShow, xlab='Alcoholism?', ylab='No Show?', main='No Shows Based on Alcoholism')
  plot(NoShowData$Handicap, NoShowData$NoShow, xlab='Handicap?', ylab='No Show?', main='No Shows Based on Handicap')
  plot(NoShowData$SmsReceived, NoShowData$NoShow, xlab='SMS Received?', ylab='No Show?', main='No Shows Based on SMS Received')
  
  
  # Box Plots & Histograms: Are NoShows coming from a particular group?
  barplot(table(NoShowData$Neighborhood))
  table(NoShowData$Neighborhood[NoShowData$NoShow=='Yes'])

  # Day of week (any day that's bad)
  table(NoShowData$AptWDay)
  barplot(NoShowData$AptWDay, NoShowData$NoShow, 
          xlab="NoShow",
          ylab='Appointment Day', 
          main='Trend of NoShows by Day of Week') 
  
  # Age vs. NoShow rate
  boxplot(NoShowData$Age~ NoShowData$NoShow, 
          xlab="NoShow",
          ylab='Patient Age', 
          main='Trend of NoShows by Patient Age')

  # DaysScheduledAhead vs. NoShow rate
  hist(as.integer(NoShowData[NoShowData$NoShow=='No',]$DaysScheduledAhead), breaks = 130)
  boxplot(as.integer(NoShowData$DaysScheduledAhead)~NoShowData$NoShow, 
          xlab="NoShow",
          ylab='Days Appointment Scheduled In Advance', 
          main='Trend of NoShows by Schedule')
  
    

  

# Splitting the data for training and testing
  trainSize <- floor(0.75 * nrow(NoShowData))
  set.seed(456)
  trainInds <- sample(seq_len(nrow(NoShowData)), size=trainSize)
  train <- NoShowData[trainInds,]
  test <- NoShowData[-trainInds,]
  yColInd <- grep('NoShow', names(NoShowData))
  trainX <- train[-yColInd]
  trainY <- train[[yColInd]]
  testX <- test[-yColInd]
  testY <- test[[yColInd]]

# Define functions to calculate F1 score
  TP <- function(predictions, actual) {
    return(sum((predictions == 'Yes') & actual == 'Yes'))
  }
  
  TN <- function(predictions, actual) {
    return(sum((predictions == 'No') & actual == 'No'))
  }
  
  FP <- function(predictions, actual) {
    return(sum((predictions == 'Yes') & actual == 'No'))
  }
  
  FN <- function(predictions, actual) {
    return(sum((predictions == 'No') & actual == 'Yes'))
  }
  
  Recall <- function(predictions, actual) {
    tp <- TP(predictions, actual)
    return(tp/(tp + FN(predictions, actual)))
  }
  
  Precision <- function(predictions, actual) {
    tp <- TP(predictions, actual)
    return(tp/(tp + FP(predictions, actual)))
  }
  
  F1 <- function(predictions, actual) {
    return(2/(1/Recall(predictions, actual) + (1/Precision(predictions, actual))))
  }
  

############################################################################
# LOGISTIC REGRESSION
model_logreg <- glm(NoShow~., data=train, family=binomial(link='logit'))
summary(model_logreg)

# based on the results from the summary above, we select only significant predictors
model_logreg <- glm(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=train, family=binomial(link='logit'))
# what do these plots tell us?
plot(model_logreg)

predict_logreg <- predict(model_logreg, testX, type="response")

# see how the logist regression performed
predict_logreg <- cut(predict_logreg, breaks=c(-Inf, 0.5, Inf), labels=c('No', 'Yes'))

# Confusion matrix for logistic regression
table(testY, predict_logreg, dnn=c("actual", "predicted"))
# F1 score for logistic regression
f1_logreg <- F1(predict_logreg, testY)


############################################################################
# RIDGE REGRESSION
library(glmnet)
xRidgeTrain <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=train)
yRidgeTrain <- as.factor(train$NoShow)
model_ridge <- glmnet(xRidgeTrain, yRidgeTrain, alpha=0, lambda=10^seq(10, -2, length=100), family="binomial")
cv.ridge <- cv.glmnet(xRidgeTrain, yRidgeTrain, alpha=0, family="binomial")
bestRidgeLambda <- cv.ridge$lambda.min
xRidgeTest <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=test)
predict_ridge <- predict(model_ridge, alpha=0, s=bestRidgeLambda, newx=xRidgeTest)
predict_ridge <- cut(predict_ridge, breaks=c(-Inf, 0.5, Inf), labels=c('No', 'Yes'))

# Confusion matrix for logistic regression
table(testY, predict_ridge, dnn=c("actual", "predicted"))
# F1 score for logistic regression
f1_ridge <- F1(predict_ridge, testY)


############################################################################
# LASSO REGRESSION
xLassoTrain <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=train)
yLassoTrain <- as.factor(train$NoShow)
model_lasso <- glmnet(xLassoTrain, yLassoTrain, alpha=1, lambda=10^seq(10, -2, length=100), family="binomial")
cv.lasso <- cv.glmnet(xLassoTrain, yLassoTrain, alpha=1, family="binomial")
bestLassoLambda <- cv.lasso$lambda.min
xLassoTest <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=test)
predict_lasso <- predict(model_lasso, alpha=1, s=bestLassoLambda, newx=xLassoTest)
predict_lasso <- cut(predict_lasso, breaks=c(-Inf, 0.5, Inf), labels=c('No', 'Yes'))

# Confusion matrix for logistic regression
table(testY, predict_lasso, dnn=c("actual", "predicted"))
# F1 score for logistic regression
f1_lasso <- F1(predict_lasso, testY)

############################################################################
# LASSO REGRESSION

  
#KNN?