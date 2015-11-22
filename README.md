# Project Assignment
Course Project Repository

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 
#
1. a tidy data set as described below, 
2. a link to a Github repository with your script for performing the analysis, and 
3. a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following : 
Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement. 
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names. 
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# User Manual

First of all script create folder where store zip data. Then file was downloaded and unzipped.

Set the working dir on data folder and import data in workspace.

Variables name are overwritten using feature variable.

Using grep function I look for variables that contain words "mean" and "std" 

A tabel call data_selected is the result of row binding of both test and train datase using only selected vars.

Gsub function acts on varname in order to properly change vars in more descriptive way.

Tidy Dataset was created using dplyr library group_by and summarise each sole the problem of identifying the mean for each activity.

A txtx file was generated using write.table as suggested






