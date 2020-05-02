## Peer-graded Assignment: Getting and Cleaning Data Course Project
## Package Required : dplyr
library(dplyr)

# download zip file containing data if it hasn't already been downloaded
zUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zFile <- "UCI HAR Dataset.zip"

if (!file.exists(zFile)) {
  download.file(zUrl, zFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zFile)
}

#feature and activity df
feature <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities<- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

# reading test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feature$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

# reading train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feature$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Task 1: Merge test and train data
m_1 <- rbind(x_train, x_test)
m_2 <- rbind(y_train, y_test)
m_3 <- rbind(subject_train, subject_test)
Merged_Data <- cbind(m_3, m_2, m_1)

# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
Tidy_Data <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# Task 3: Uses descriptive activity names to name the activities in the data set.
Tidy_Data$code <- activities[Tidy_Data$code, 2]

# Task 4 :Appropriately labels the data set with descriptive variable names.
names(Tidy_Data)[2] = "activity"
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))

names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)

# Task 5 : Create a independent  tidy data set
Final_Data <- Tidy_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Final_Data, "FinalData.txt", row.name=FALSE)



