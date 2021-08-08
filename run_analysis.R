#####
# Coursera Getting and Cleaning Data: Peer-graded assignment
#####

#####
# set work directory
#####
setwd("C:/Users/kaihuis/Desktop/UMDresearch/study/coursera_ds_certificate/3_GettingAndCleaningData/data")
#####

#####
# read library
#####
library(tidyverse)

#####

#####
# read data
#####
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("featureid","featurename"))
str(features)
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityid", "activity"))
# testing data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
names(x_test) <- features$featurename
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity")

# training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$featurename)
names(x_train) <- features$featurename
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity")
#####

#####
# task 1: merges training and testing sets to create one data set
#####
merge_subject <- rbind(subject_train, subject_test)
merge_feature <- rbind(x_train, x_test)
merge_activity <-rbind(y_train, y_test)
merged_data <-cbind(merge_subject, merge_feature, merge_activity)
#####

#####
# task 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#####
index_mean <- grep("mean\\(\\)",colnames(merged_data))
mean_data <- merged_data[,index_mean]

index_std <-grep("std\\(\\)",colnames(merged_data))
std_data <- merged_data[,index_std]

mean_std_data <- cbind(mean_data, std_data)
colnames(merged_data)

merged_data_new <- subset(merged_data[c(1,563,index_mean, index_std)])
str(merged_data)
#####

#####
# task 3: Uses descriptive activity names to name the activities in the data set
#####
merged_data_new %>%
      rename(activityid = activity) %>%
      left_join(activities, by = "activityid") %>%
      select(-activityid) -> activitydata

#####

#####
# task 4: Appropriately labels the data set with descriptive variable names. 
#####
names(activitydata)<-gsub("^t", "time", names(activitydata))
names(activitydata)<-gsub("^f", "frequency", names(activitydata))
names(activitydata)<-gsub("Acc", "Accelerometer", names(activitydata))
names(activitydata)<-gsub("Gyro", "Gyroscope", names(activitydata))
names(activitydata)<-gsub("Mag", "Magnitude", names(activitydata))
names(activitydata)<-gsub("BodyBody", "Body", names(activitydata))
names(activitydata)
#####

##### 
# task 5: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#####
newdata <- activitydata %>%
      group_by(subject, activity) %>%
      summarise_all(funs(mean))
write.table(newdata, "newdata.txt", row.name=FALSE)
#####


