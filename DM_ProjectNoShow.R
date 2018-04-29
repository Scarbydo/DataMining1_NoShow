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

  
# Data Cleaning ####
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

  
# visualize the data ####
  #Create to Data Frame using dplyr
  #NoShowData_df <- tbl_df(NoShowData)
  
  chartColors = c("grey", "red") #Red is for nowshows
  
  #patient health characteristics by age to check validity of data
  barplot(table(NoShowData$Age[NoShowData$Alcoholism==1]), 
          xlab ='Age', ylab = 'Has Alcoholism')
  barplot(table(NoShowData$Age[NoShowData$Hypertension==1]),
          xlab ='Age', ylab = 'Has Hypertension')
  barplot(table(NoShowData$Age[NoShowData$Diabetes==1]),
          xlab ='Age', ylab = 'Has Diabetes')
  
  
  # See the counts of our categorical variables
  dataCounts = list()
  dataCounts$Gender <- table(NoShowData$NoShow, NoShowData$Gender)
  dataCounts$Scholarship <- table(NoShowData$NoShow, NoShowData$Scholarship)
  dataCounts$Hypertension <- table(NoShowData$NoShow, NoShowData$Hypertension)
  dataCounts$Diabetes <- table(NoShowData$NoShow, NoShowData$Diabetes)
  dataCounts$Alcoholism <- table(NoShowData$NoShow, NoShowData$Alcoholism)
  dataCounts$Handicap <- table(NoShowData$NoShow, NoShowData$Handicap)
  dataCounts$SmsReceived <- table(NoShowData$NoShow, NoShowData$SmsReceived)
  dataCounts$AptWDay <- table(NoShowData$NoShow, NoShowData$AptWDay)
  dataCounts 
  #Percentge plots
  # Compare NoShow = Yes vs. Noshow = No
  plot(NoShowData$Gender, NoShowData$NoShow, 
       xlab='Gender', ylab='No Show?', 
       main='No Shows Based on Gender',
       col=chartColors)
  plot(NoShowData$Scholarship, NoShowData$NoShow, 
       xlab='Scholarship?', ylab='No Show?', 
       main='No Shows Based on Scholarship',
       col=chartColors)
  plot(NoShowData$Hypertension, NoShowData$NoShow, 
       xlab='Hypertension?', ylab='No Show?', 
       main='No Shows Based on Hypertension',
       col=chartColors)
  plot(NoShowData$Diabetes, NoShowData$NoShow, 
       xlab='Diabetes?', ylab='No Show?', 
       main='No Shows Based on Diabetes',
       col=chartColors)
  plot(NoShowData$Alcoholism, NoShowData$NoShow, 
       xlab='Alcoholism?', ylab='No Show?', 
       main='No Shows Based on Alcoholism',
       col=chartColors)
  plot(NoShowData$Handicap, NoShowData$NoShow, 
       xlab='Handicap?', ylab='No Show?', 
       main='No Shows Based on Handicap',
       col=chartColors)
  plot(NoShowData$SmsReceived, NoShowData$NoShow, 
       xlab='SMS Received?', ylab='No Show?', 
       main='No Shows Based on SMS Received',
       col=chartColors)
  plot(NoShowData$AptWDay, NoShowData$NoShow, 
       xlab='SMS Received?', ylab='No Show?', 
       main='No Shows Based on SMS Received',
       col=chartColors)
  
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
         fill =chartColors, 
         legend = c("NoShow=No","NoShow=Yes"))
  
  # Number of appointments per patient
  aptsPerPatient = table(NoShowData$PatientId)
  aptsPerPatient = sort(aptsPerPatient, decreasing = T)
  mostappointments = as.integer(max(table(NoShowData$PatientId)))
  barplot(aptsPerPatient, 
          ylim=c(0,10+mostappointments),
          xaxt='n',
          xlab="Patient ID", 
          ylab="# of Appointments",
          main="Appointments per patient")  
  mean(aptsPerPatient)
  median(aptsPerPatient)
  

  
  
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
  
    

# Splitting the data for training and testing ####
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

  # different format for the glmnet models
  xTrain <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=train)
  yTrain <- as.factor(train$NoShow)
  xTest <- model.matrix(NoShow~Age+Scholarship+Hypertension+Diabetes+Alcoholism+SmsReceived+DaysScheduledAhead+AptWDay, data=test)

# Define functions to calculate F1 score ####
  # Positive = No, the patient was NOT a no show
  # Negative = Yes, the patient was a no show
  #True Positives
  TP <- function(predictions, actual) {
    return(sum((predictions == 'No') & (actual == 'No')))
  }
  
  #True Negative
  TN <- function(predictions, actual) {
    return(sum((predictions == 'Yes') & (actual == 'Yes')))
  }
  
  #False Positive
  FP <- function(predictions, actual) {
    return(sum((predictions == 'No') & (actual == 'Yes')))
  }
  
  #False Negative
  FN <- function(predictions, actual) {
    return(sum((predictions == 'Yes') & (actual == 'No')))
  }
  
  Recall <- function(predictions, actual) {
    tp <- TP(predictions, actual)
    return(tp/(tp + FN(predictions, actual)))
  }
  
  Precision <- function(predictions, actual) {
    tp <- TP(predictions, actual)
    return(tp/(tp + FP(predictions, actual)))
  }
  
  #False Negative Rate
  FNR <- function(predictions, actual) {
    fn <- FN(predictions, actual)
    return(fn / (fn + TP(predictions, actual)))
  }
  
  #False Positive Rate
  FPR <- function(predictions, actual) {
    fp <- FP(predictions, actual)
    return(fp / (fp + TN(predictions, actual)))
  }
  
  #F1 Score
  F1 <- function(predictions, actual) {
    return(2/(1/Recall(predictions, actual) + (1/Precision(predictions, actual))))
  }
  


# LOGISTIC REGRESSION ############################################################################
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



# ELASTIC NET REGRESSION ############################################################################
library(glmnet)
elasticF1s <- c()
for (i in seq(0, 1, by=0.1)) {
  fElasticRegression <- glmnet(xTrain, yTrain, alpha=i, lambda=10^seq(10, -2, length = 100), family='binomial')
  cv.elastic <- cv.glmnet(xTrain, yTrain, alpha=i, family='binomial')
  bestElasticLambda <- cv.elastic$lambda.min
  fPredictions <- predict(fElasticRegression, alpha=i, s=bestElasticLambda, newx=xTest)
  fPredictions <- cut(fPredictions, breaks=c(-Inf, 0.5, Inf), labels=c('No', 'Yes'))
  f1 <- F1(fPredictions, testY)
  elasticF1s <- c(elasticF1s, f1)
}

maxElasticF1 <- max(elasticF1s)
indexOfMaxF1 <- match(maxElasticF1, elasticF1s)
elasticAlpha <- seq(0,1,by=0.1)[indexOfMaxF1]

fElasticRegression <- glmnet(xTrain, yTrain, alpha=elasticAlpha, lambda=10^seq(10, -2, length = 100), family='binomial')
cv.elastic <- cv.glmnet(xTrain, yTrain, alpha=elasticAlpha, family='binomial')
bestElasticLambda <- cv.elastic$lambda.min
predict_elastic <- predict(fElasticRegression, alpha=elasticAlpha, s=bestElasticLambda, newx=xTest)
predict_elastic <- cut(predict_elastic, breaks=c(-Inf, 0.5, Inf), labels=c('No', 'Yes'))

# get coefficients for the best model
predict(fElasticRegression, type="coefficients", s=bestElasticLambda)

# plot the F1 values for the different alpha values
plot(seq(0, 1, by=0.1), elasticF1s, xlab='Alpha', ylab='F1', main='Alpha Values vs F1 Scores')

# Confusion matrix for elastic regression
table(testY, predict_elastic, dnn=c("actual", "predicted"))
# F1 score for elastic regression
f1_elastic <- F1(predict_elastic, testY)



# DUMB GUESS ############################################################################
dumb <- as.factor(rep('No', length(testY)))
table(dumb, testY)
f1_dumb <- F1(dumb, testY)