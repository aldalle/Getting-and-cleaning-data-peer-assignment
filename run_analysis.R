## open required package
library(dplyr)


## give variable 'filename' the stringvalue 'Coursera_DS3_Final.zip' 
filename <- "Coursera_DS3_Final.zip"


## check if archieve (Coursera_DS3_Final.zip) already exists, if not download archieve
if (!file.exists(filename)){
      fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(fileURL, filename)}  


## checking if folder (UCI HAR Dataset) already exists, if not make folder and unzip
if (!file.exists("UCI HAR Dataset")) { 
      unzip(filename)}


## assigning all the different dataframes extracted from the UCI HAR Dataset 
## giving appropriate columnnames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


## 1. merge the training and the test sets to create one data set
## combine the rows of x_train and x_test into X
X <- rbind(x_train, x_test)
## combine the rows of y_train and y_test into Y
Y <- rbind(y_train, y_test)
## combine the rows of subject_train and subject_test into subject
Subject <- rbind(subject_train, subject_test)
## combine the columns of subject, Y and X into merged_data
merged_data <- cbind(Subject, Y, X)


## 2. extract only the measurements on the mean and standard deviation for each 
## measurement in subject
tidydata <- merged_data %>% select(Subject, code, contains("mean"), contains("std"))

## 3. use descriptive activity names to name the activities in the data set
tidydata$code <- activities[tidydata$code, 2]



## 4. appropriately label the data set with descriptive variable names
names(tidydata)<-gsub("subject", "Subject", names(tidydata))
names(tidydata)<-gsub("code", "Activity", names(tidydata))
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "Standard deviation", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))


## 5. from the data set in step 4, create a second, independent tidy data set 
## with the average of each variable for each activity and each subject
finaldata <- tidydata %>%
      group_by(Subject, Activity) %>%
      summarise_all(funs(mean))
write.table(finaldata, "FinalData.txt", row.name=FALSE)


## check variable names in finaldata
str(finaldata)

## check finaldata
finaldata