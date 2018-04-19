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
  
  #check for missing data
  sum(is.na(NoShowData$PatientId))
  sum(is.na(NoShowData$AppointmentID))
  sum(is.na(NoShowData$Gender))
  sum(is.na(NoShowData$ScheduledDay))
  sum(is.na(NoShowData$AppointmentDay))
  sum(is.na(NoShowData$Age))
  sum(is.na(NoShowData$Neighbourhood))
  sum(is.na(NoShowData$Scholarship))
  sum(is.na(NoShowData$Hipertension))
  sum(is.na(NoShowData$Diabetes))
  sum(is.na(NoShowData$Alcoholism))
  sum(is.na(NoShowData$Handcap))
  sum(is.na(NoShowData$SMS_received))
  sum(is.na(NoShowData$`No-show`))
  
  
  # examin correlation between variables using pairs()
  length(NoShowData$PatientId)
  
# Performance measure: use F-Score
