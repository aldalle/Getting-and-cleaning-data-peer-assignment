The run_analysis.R script performs the data preparation and then followed by the 5 steps required as described in the course project’s definition.

DATA PREPARATION
1. Download the dataset 
Dataset downloaded from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

2. Extracted the dataset under the folder called UCI HAR Dataset

3. Assign each data to variables
3.1. features <- features.txt : 561 rows, 2 columns
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.

3.2. activities <- activity_labels.txt : 6 rows, 2 columns
List of activities performed when the corresponding measurements were taken and its codes (labels)

3.3. subject_test <- test/subject_test.txt : 2947 rows, 1 column
contains test data of 9/30 volunteer test subjects being observed

3.4. x_test <- test/X_test.txt : 2947 rows, 561 columns
contains recorded features test data

3.5. y_test <- test/y_test.txt : 2947 rows, 1 columns
contains test data of activities’code labels

3.6. subject_train <- test/subject_train.txt : 7352 rows, 1 column
contains train data of 21/30 volunteer subjects being observed

3.7. x_train <- test/X_train.txt : 7352 rows, 561 columns
contains recorded features train data

3.8. y_train <- test/y_train.txt : 7352 rows, 1 columns
contains train data of activities’code labels


FIVE REQUIRED STEPS

STEP 1. Merges the training and the test sets to create one data set
X (10299 rows, 561 columns) is created by merging x_train and x_test using rbind() function
Y (10299 rows, 1 column) is created by merging y_train and y_test using rbind() function
Subject (10299 rows, 1 column) is created by merging subject_train and subject_test using rbind() function
merged_data (10299 rows, 563 column) is created by merging Subject, Y and X using cbind() function

STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement
tidydata (10299 rows, 88 columns) is created by subsetting merged_data, selecting only columns: subject, code and the measurements on the mean and standard deviation (std) for each measurement

STEP 3. Uses descriptive activity names to name the activities in the data set
Entire numbers in code column of the tidydata replaced with corresponding activity taken from second column of the activities variable

STEP 4. Appropriately labels the data set with descriptive variable names
code column in tidydata renamed into Activities
All Acc in column’s name replaced by Accelerometer
All Gyro in column’s name replaced by Gyroscope
All BodyBody in column’s name replaced by Body
All Mag in column’s name replaced by Magnitude
All start with character f in column’s name replaced by Frequency
All start with character t in column’s name replaced by Time

STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
finaldata (180 rows, 88 columns) is created by sumarizing tidydata taking the means of each variable for each activity and each subject, after groupped by subject and activity.
Export finaldata into FinalData.txt file.