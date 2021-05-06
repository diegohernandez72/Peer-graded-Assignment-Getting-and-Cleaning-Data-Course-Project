library(tidyverse)

filename <- "Coursera_DS3_Final.zip"

## Download and extract the dataset  ####
# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

## load all data frames ####
features <- read.table("UCI HAR Dataset/features.txt" ,col.names = c("n","feature"))
head(features)
dim(features)
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
head(activity)
dim(activity)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
head(subject_test)
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
head(x_test)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
head(y_test)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
head(subject_train)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
head(x_train)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
head(y_train)

## 1. Merges the training and the test sets to create one data set.
x.merge <- rbind(x_train, x_test)
y.merge <- rbind(y_train, y_test)
subject.merge <- rbind(subject_train, subject_test)
data.merge <- cbind(subject.merge, y.merge, x.merge)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
tidydata <- data.merge %>% select(subject, code, contains("mean"), contains("std"))
dim(tidydata)
## 3. Uses descriptive activity names to name the activities in the data set
tidydata$code <- activity[tidydata$code, 2]

## 4. Appropriately labels the data set with descriptive variable names.
names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

## 5. From the data set in step 4, creates a second, independent tidy data 
## set with the average of each variable for each activity and each subject.
finalData <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(finalData, "finalData.txt", row.name=FALSE)
