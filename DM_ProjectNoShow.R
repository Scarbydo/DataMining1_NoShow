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
  
# Data Exploration
names(NoShowData)
str(NoShowData)
dim(NoShowData)

names(NoShowData) <- list("PatientId","AppointmentId","Gender","ScheduledDay","AppointmentDay","Age","Neighborhood","Scholarship","Hypertension","Diabetes","Alcoholism","Handicap","SmsReceived","NoShow")

# check for missing data
sapply(NoShowData, function(x) sum(is.na(x)))

# check for number of unique values in each column
sapply(NoShowData, function(x) length(unique(x)))

# 81 of the 110527 neighborhoods are unique. let's factorize it,
# along with Gender and NoShow
NoShowData$Neighborhood <- as.factor(NoShowData$Neighborhood)
NoShowData$Gender <- as.factor(NoShowData$Gender)
NoShowData$NoShow <- as.factor(NoShowData$NoShow)

# appointment ID is not helpful
NoShowData$AppointmentId <- NULL

# we don't need the time in the SCheduleDay, so let's drop it
NoShowData$ScheduledDay <- trunc(NoShowData$ScheduledDay, units='days')
# create a new column with the difference between the day scheduled and the appointment day
NoShowData['DaysScheduledAhead'] <- difftime(NoShowData$AppointmentDay, NoShowData$ScheduledDay, units='days')

# need to see if there are outliers in DaysScheduledAhead and Age


# splitting the data for training and testing
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

# Performance measure: use F-Score
model <- glm(NoShow~., data=train, family=binomial(link='logit'))
summary(model)
anova(model, test='Chisq')
