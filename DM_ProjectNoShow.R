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

# Import Data
library(readr)
NoShowData <- read_csv("KaggleV2-May-2016.csv")
#View(NoShowData)
  
# Data Exploration
typeof(NoShowData)
names(NoShowData)
str(NoShowData)
dim(NoShowData)

NoShowData$Gender <- as.factor(NoShowData$Gender)
NoShowData$`No-show` <- as.factor(NoShowData$`No-show`)
names(NoShowData) <- list("PatientId","AppointmentId","Gender","ScheduledDay","AppointmentDay","Age","Neighborhood","Scholarship","Hypertension","Diabetes","Alcoholism","Handicap","SmsReceived","NoShow")

numUniqueNeighborhoods <- length(unique(NoShowData$Neighborhood))
# 81 of the 110527 are unique. factorize it?

#check for missing data
sum(is.na(NoShowData$PatientId))
sum(is.na(NoShowData$AppointmentId))
sum(is.na(NoShowData$Gender))
sum(is.na(NoShowData$ScheduledDay))
sum(is.na(NoShowData$AppointmentDay))
sum(is.na(NoShowData$Age))
sum(is.na(NoShowData$Neighborhood))
sum(is.na(NoShowData$Scholarship))
sum(is.na(NoShowData$Hypertension))
sum(is.na(NoShowData$Diabetes))
sum(is.na(NoShowData$Alcoholism))
sum(is.na(NoShowData$Handicap))
sum(is.na(NoShowData$SmsReceived))
sum(is.na(NoShowData$NoShow))

trainSize <- floor(0.75 * nrow(NoShowData))
set.seed(456)
trainInds <- sample(seq_len(nrow(NoShowData)), size=trainingSetSize)
train <- NoShowData[trainInds,]
test <- NoShowData[-trainInds,]
yColInd <- grep('NoShow', names(NoShowData))
trainX <- train[-yColInd]
trainY <- train[yColInd]
testX <- test[-yColInd]
testY <- test[yColInd]

# examine correlation between variables using pairs()
# I don't think we need to check this out, since we're categorical

# Performance measure: use F-Score
