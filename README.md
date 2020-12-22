Peer-graded Assignment: Getting and Cleaning Data Course Project
This repository is a submission created by Axana Dalle for Getting and Cleaning Data course project. It has the instructions on how to run analysis on Human Activity recognition dataset.

1. Dataset
I used the dataset "Human Activity Recognition Using Smartphones (UCI HAR)" downloaded from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

2. Files
2.1. CodeBook.md is a code book that describes the variables, the data and any transformations or work that I performed to clean the data.

2.2. run_analysis.R performs the data preparation and then followed by the 5 steps required as described in the course project’s definition:
- Merges the training and the test sets to create one data set.
- Extracts only the measurements on the mean and standard deviation for each measurement.
- Uses descriptive activity names to name the activities in the data set
- Appropriately labels the data set with descriptive variable names.
- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

2.3. FinalData.txt is the exported final data after going through all the sequences described above.