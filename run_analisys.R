# Course Project Assignement 
# Getting and Cleaning Data
# Coursera
#
# Author    : M.Jacono
# Email     :marco.jacono@gmail.com
# 
# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
#
# Data Project
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# Clean up workspace
rm(list=ls())

# HINT : Set workspace session where script was saved
library(dplyr)
# Create Data Folder and don't show warning if Data Dir already exist
dir.create("./Data/", showWarnings = FALSE)

# Download ZIp File in Data Folder, 60 Mb are downloaded
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "./Data/dataset.zip"
download.file(url,destfile)

# Unzip Data
unzip(destfile)

# Set WorkingDirectory in extracted folder
setwd("./UCI HAR Dataset/");

# 1. Merge the training and the test sets to create one data set.

# Import data in R workspace as table with read.table command

# Import features and activity Labels
features      = read.table('./features.txt',header=FALSE) #imports features.txt
activityLabels = read.table('./activity_labels.txt',header=FALSE) #imports activity_labels.txt

# Import TRAINING DataSet
subjectTrain = read.table('./train/subject_train.txt',header=FALSE) 
xTrain       = read.table('./train/X_train.txt',header=FALSE) 
yTrain       = read.table('./train/y_train.txt',header=FALSE) 

# Import TEST DataSet
subjectTest   = read.table('./test/subject_test.txt',header=FALSE) 
xTest         = read.table('./train/X_test.txt',header=FALSE) 
yTest         = read.table('./train/y_test.txt',header=FALSE) 

# Every data now it's in workspace
# About Train (but it's the same for test) we can observe that :
# xTrain 7532 obs x 561 variables thar are features
# subjectTrain is a vector with id_subject and yTrain a vector with id_activity. Activity Labels are in activityLabels Var


# Now we can set the column names of each table 
colnames(activityLabels)  = c('id_activity','activity_label')
colnames(subjectTrain)  = "id_subject"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "id_activity"


# Try to select only varname with mean and std using grep
col_selected <- grep(".*mean.*|.*std.*", features[,2])
# column bind of data
training_data_selected = cbind(yTrain,subjectTrain,xTrain[,col_selected])

# Do the same from test folder
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Set names
colnames(subjectTest) = "id_subject"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "id_activity"

# Create the final test set by merging the xTest, yTest and subjectTest data
test_data_selected = cbind(yTest,subjectTest,xTest[,col_selected])

# row bind of two tables var names are consistent
data_selected = rbind(training_data_selected,test_data_selected)


# 3. Use descriptive activity names to name the activities in the data set

merged_data_selected = merge(merged_data_selected,activityLabels,by="id_activity",all.x = TRUE)

# Updating the colNames vector to include the new column names after merge
variables_name  = colnames(merged_data_selected) 

# 4. Appropriately label the data set with descriptive activity names. 

variables_name <- gsub("-meanFreq()", "_mean_freq", variables_name) # substitutes "-meanFreq()" with ".mean.freq" 
variables_name <- gsub("-mean()", "_mean", variables_name)  # substitutes "-mean" with ".mean" 
variables_name <- gsub("-std()", "_std", variables_name)    # substitutes "-std" with ".std" 
variables_name <- gsub("[-]", "", variables_name)          # substitutes "-" with "." 
variables_name <- gsub("[()]", "", variables_name)          # removes "()" 

# Change varname
colnames(merged_data_selected) <- variables_name

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create dataset using chaining and dplyr library

tidy_dataset <- merged_data_selected %>%
  group_by(activity_label) %>%
  summarise_each(funs(mean),3:81)

# Export tidy_dataset
write.table(tidy_dataset, './tidyD_data.txt',row.names=FALSE,sep='\t')
