## First set the working directory

setwd("~/Coursera/Data Science/Getting&Cleaning_Data/Project")

## Create a dir called "data"if it does not exist
## Download the file and put it in the "data" directory

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")



## Unzip the File

unzip(zipfile="./data/Dataset.zip", exdir="./data")

## unzipped files are in the folder "UCI HAR Dataset"

##  Get the list of the files
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

## Read data from the files into the variables

## Read the Activity files

dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)


## Read the Subject files

dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)


## Read Fearures files

dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

#************** Assignment 1 ***************

##Merges the training and the test sets to create one data set

## 1.Concatenate the data tables by rows
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)


## 2.set names to variables

names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2


## 3. Merge columns to get the data frame "MasterData" for all data

dataCombine <- cbind(dataSubject, dataActivity)
MasterData <- cbind(dataFeatures, dataCombine)

#************** Assignment 2 ***************
## Extracts only the measurements on the mean and standard deviation for each measurement
## Subset Name of Features by measurements on the mean and standard deviation
## i.e taken Names of Features with "mean()" or "std()"

NamesFeatSubdata<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

## Subset the data frame MasterData by seleted names of Features

selectedNames<-c(as.character(NamesFeatSubdata), "subject", "activity" )
MasterData<-subset(MasterData,select=selectedNames)


#************** Assignment 3 ***************
## Uses descriptive activity names to name the activities in the data set

MasterData$activity <- as.character(MasterData$activity)
MasterData$activity[MasterData$activity == 1] <- "WALKING"
MasterData$activity[MasterData$activity == 2] <- "WALKING_UPSTAIRS"
MasterData$activity[MasterData$activity == 3] <- "WALKING_DOWNSTAIRS"
MasterData$activity[MasterData$activity == 4] <- "SITTING"
MasterData$activity[MasterData$activity == 5] <- "STANDING"
MasterData$activity[MasterData$activity == 6] <- "LAYING"
MasterData$activity <- as.factor(MasterData$activity)


#************** Assignment 4 ***************
## Appropriately labels the data set with descriptive variable names.
## In the former part, variables activity and subject and names of the activities 
## have been labelled using descriptive names.In this part, Names of Feteatures will 
## labelled using descriptive variable names.

##  prefix t is replaced by time
##  Acc is replaced by Accelerometer
##  Gyro is replaced by Gyroscope
##  prefix f is replaced by frequency
##  Mag is replaced by Magnitude
##  BodyBody is replaced by Body

names(MasterData)<-gsub("^t", "time", names(MasterData))
names(MasterData)<-gsub("^f", "frequency", names(MasterData))
names(MasterData)<-gsub("Acc", "Accelerometer", names(MasterData))
names(MasterData)<-gsub("Gyro", "Gyroscope", names(MasterData))
names(MasterData)<-gsub("Mag", "Magnitude", names(MasterData))
names(MasterData)<-gsub("BodyBody", "Body", names(MasterData))


#************** Assignment 5 ***************
## From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.

library(plyr);
library(data.table);
library(dplyr);
library(tidyr);
library(LaF);

MasterDAta2 <- data.table(MasterData)
#This takes the mean of every column broken down by participants and activities

TidyData <- group_by(MasterData, subject, activity) %>% summarise_all(funs(mean)) %>% gather(measurement, mean, -activity, -subject)

write.table(TidyData, file = "Tidy_DaTa.txt", row.names = FALSE)

##END
