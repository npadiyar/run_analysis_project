# Coursera Getting and Cleaning Data Course Project
# Nagesh Padiyar
# 07/23/2016
#
# runAnalysis.r File Description:
#
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


# 1. Merge the training and the test sets to create one data set.

# Cleanup the workspace
rm(list=ls())

# load the following packages
library(plyr); # load plyr first, then dplyr
library(dplyr); 
install.packages("data.table"); 
install.packages("reshape2");
library("data.table");
library("reshape2");

# Set the working directory to the appropriate location where the files reside
setwd('/Users/npadiyar/Documents/UCI HAR Dataset/');

# Read dataset from features.txt 
features <- read.table('./features.txt',header=FALSE);
View(features) # View the features 

# Read dataset from activity_labels.txt 
activityLabels <- read.table('./activity_labels.txt',header=FALSE);
View(activityLabels)

# Read dataset from train/subject_train.txt 
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE);
View(subjectTrain)

# Read dataset from train/X_train.txt 
xTrain <- read.table('./train/X_train.txt',header=FALSE);
View(xTrain)

# Read dataset from train/Y_train.txt 
yTrain <- read.table('./train/Y_train.txt',header=FALSE);

# Column Names for datasets
colnames(activityLabels) = c('activityId','activityLabel');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";

# Create trainingData dataset by merging yTrain, subjectTrain and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);
View(trainingData)

# Read datasets from test data files
subjectTest <- read.table('./test/subject_test.txt',header=FALSE);
xTest <- read.table('./test/X_test.txt',header=FALSE);
yTest <- read.table('./test/y_test.txt',header=FALSE);

# Assign appropriate Column Names for the test datasets
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2]; 
colnames(yTest) = "activityId";

# Create testData dataset by merging yTest, subjectTest and xTest
testData = cbind(yTest,subjectTest,xTest);
View(testData);

# Create Final dataset by combining testData and trainingData
finalData = rbind(testData, trainingData);

# 2. Extract only the measurements on the mean and standard deviation for each measurement

# Get only those columns which have mean() or std() in their names
mean_col_index <- grep("mean",names(finalData),ignore.case=TRUE);
mean_col_names <- names(finalData)[mean_col_index];
View(mean_col_names);
std_col_index <- grep("std",names(finalData),ignore.case=TRUE);
std_col_names <- names(finalData)[std_col_index];
View(std_col_names);
finalmeanstdData <-finalData[,c("subjectId","activityId",mean_col_names,std_col_names)];
View(finalmeanstdData);
str(finalmeanstdData);

# 3. Use descriptive activity names to name the activities in the data set

finaldescrData <- merge(activityLabels,finalmeanstdData,by.x="activityId",by.y="activityId",all=TRUE);
View(finaldescrData);

# 4. Appropriately label the data set with descriptive activity names.

names(finaldescrData)<-gsub("^t", "time", names(finaldescrData));
names(finaldescrData)<-gsub("^f", "frequency", names(finaldescrData));
names(finaldescrData)<-gsub("Acc", "Accelerometer", names(finaldescrData));
names(finaldescrData)<-gsub("Gyro", "Gyroscope", names(finaldescrData));
names(finaldescrData)<-gsub("Mag", "Magnitude", names(finaldescrData));
names(finaldescrData)<-gsub("BodyBody", "Body", names(finaldescrData));

# verify that the descriptive column names have been set correctly
names(finaldescrData) 

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

finalmeltedData <- melt(finaldescrData,id=c("activityId","activityLabel","subjectId"));
View(finalmeltedData);

finalmeanData <- dcast(finalmeltedData,activityId + activityLabel + subjectId ~ variable,mean);
View(finalmeanData);

write.table(finalmeanData,"./final_mean_data.txt")











