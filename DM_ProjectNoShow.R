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
  
  # We don't need the time in the SCheduleDay, so let's drop it 
  NoShowData$ScheduledDay <- as.POSIXct(trunc(NoShowData$ScheduledDay, units='days'))
  
  # Create a new column with the difference between the day scheduled and the appointment day
  NoShowData['DaysScheduledAhead'] <- difftime(NoShowData$AppointmentDay, NoShowData$ScheduledDay, units='days')
  
  # Check Days Scheduled Ahead to see if the values make sense
  hist(as.integer(NoShowData$DaysScheduledAhead), breaks = 130)
  table(as.integer(NoShowData$DaysScheduledAhead))
  ## ERROR: Some Appointments appear to be scheduled AFTER the appoinment has occurred!  
  ##        Dropping the time in ScheduledDay does not cause the error.
  ##        We should find a way to exclude these.
  # Compare

    
  # Check age range to see if it makes sense
  range(NoShowData$Age)
  boxplot(NoShowData$Age~ NoShowData$NoShow)
  boxplot(NoShowData$Gender~ NoShowData$NoShow)
  

# visualize the data
  plot(NoShowData$Gender, NoShowData$NoShow, xlab='Gender', ylab='No Show?', main='No Shows Based on Gender')
  
  ## JUSTIN: This plot is not very effictive.  Maybe a histogram or boxplot of noshows by Days Scheduled Ahead is better
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
  
  # visualization continued: Histograms - are no shows coming from a particular group?
  # NoShow = Yes and by neighborhood
  # NoShow = Yes and day of week
  # NoShow = Yes and month
  # NoShow = Yes and by age
  # NoShow = Yes and by difference in Appointment booked and Appointment time
  # Number of appointments per patient & % no-show
  # Look at DaysScheduledAhead vs. No show rate
  
  # See the counts of our categorical variables
  table(NoShowData$Gender)
  table(NoShowData$Scholarship)
  table(NoShowData$Hypertension)
  table(NoShowData$Diabetes)
  table(NoShowData$Alcoholism)
  table(NoShowData$Handicap)
  table(NoShowData$SmsReceived)
  # The skewed counts might mean something...
  

# Splitting the data for training and testing
  trainSize <- floor(0.75 * nrow(NoShowData))
  set.seed(456)
  trainInds <- sample(seq_len(nrow(NoShowData)), size=trainSize)
  train <- NoShowData[trainInds,]
  test <- NoShowData[-trainInds,]
  yColInd <- grep('NoShow', names(NoShowData))
  trainX <- train[-yColInd]
  trainY <- train[yColInd]
  testX <- test[-yColInd]
  testY <- test[yColInd]

# examine correlation between variables using pairs()
# I don't think we need to check this out, since we're categorical


# JUSTIN: 
  #  Agree GLM logit is best for classification
  #  Should we use LASSO or RIDGE to select variables for our model?
# Performance measure: use F-Score
model <- glm(NoShow~., data=train, family=binomial(link='logit'))
summary(model)
anova(model, test='Chisq')
