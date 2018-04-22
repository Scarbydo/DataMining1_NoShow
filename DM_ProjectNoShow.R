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
# No=1  Yes=2
NoShowData$NoShow <- as.factor(NoShowData$NoShow)
# We should probably turn these into yes/no
NoShowData$Scholarship <- as.factor(NoShowData$Scholarship)
NoShowData$Hypertension <- as.factor(NoShowData$Hypertension)
NoShowData$Diabetes <- as.factor(NoShowData$Diabetes)
NoShowData$Alcoholism <- as.factor(NoShowData$Alcoholism)
NoShowData$Handicap <- as.factor(NoShowData$Handicap)
NoShowData$SmsReceived <- as.factor(NoShowData$SmsReceived)

# appointment ID is not helpful
NoShowData$AppointmentId <- NULL

# we don't need the time in the SCheduleDay, so let's drop it
NoShowData$ScheduledDay <- as.POSIXct(trunc(NoShowData$ScheduledDay, units='days'))
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

# visualize the data
plot(NoShowData$Gender, NoShowData$NoShow, xlab='Gender', ylab='No Show?', main='No Shows Based on Gender')
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

# See the counts of our categorical variables
table(NoShowData$Gender)
table(NoShowData$Scholarship)
table(NoShowData$Hypertension)
table(NoShowData$Diabetes)
table(NoShowData$Alcoholism)
table(NoShowData$Handicap)
table(NoShowData$SmsReceived)
# The skewed counts might mean something...

# Performance measure: use F-Score
model <- glm(NoShow~., data=train, family=binomial(link='logit'))
summary(model)
anova(model, test='Chisq')
