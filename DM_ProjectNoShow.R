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
  
  
  # examin correlation between variables using pairs()
  
  
# Performance measure: use F-Score
