##step1: Data download and unzip 

# string variables for file download
fileName <- "UCIdata.zip"
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(fileName)){
        download.file(url,fileName, mode = "wb") 
}

# File unzip verification. If the directory does not exist, unzip the downloaded file.
if(!file.exists(dir)){
        unzip("UCIdata.zip", files = NULL, exdir=".")
}


if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("dplyr")) {
        install.packages("dplyr")
}

library("data.table")
library("reshape2")
#step 2:read data
# Load: activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Load: data column names
features <- read.table("./UCI HAR Dataset/features.txt")
# Load: test data
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt") 
# Load: training data
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt") 
#each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt") 
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
#Test labels
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")  
#Training labels
y_train <- read.table("UCI HAR Dataset/train/y_train.txt") 


##setp 3: 1.Merges the training and the test sets to create one data set x.

x <- rbind(x_train, x_test) 
colnames(x)<-features[,2]

##step 4:##2.Extracts only the measurements on the mean(mean(): Mean value) and standard deviation(std(): Standard deviation) for each measurement.

ExtraData<-x[,grep("mean()|std()",colnames(x),ignore.case = T)]




##step 5:# 3.Uses descriptive activity names to name the activities in the data set
y <- rbind(y_train, y_test)
colnames(y)<-"Act"
Subject <- rbind(subject_train, subject_test)
colnames(Subject)<-"Id"
ExtraData <- cbind(Subject, y, ExtraData)

ExtraData$Act<-activity_labels[ExtraData$Act,2]

##step 6: #4.Appropriately labels the data set with descriptive variable names.
names(ExtraData)[1] = "Subject"
names(ExtraData)[2] = "Activity"
names(ExtraData)<-gsub("Acc", "Accelerometer", names(ExtraData))
names(ExtraData)<-gsub("Gyro", "Gyroscope", names(ExtraData))
names(ExtraData)<-gsub("BodyBody", "Body", names(ExtraData))
names(ExtraData)<-gsub("Mag", "Magnitude", names(ExtraData))
names(ExtraData)<-gsub("^t", "Time", names(ExtraData))
names(ExtraData)<-gsub("^f", "Frequency", names(ExtraData))
names(ExtraData)<-gsub("tBody", "TimeBody", names(ExtraData))
names(ExtraData)<-gsub("-mean()", "Mean", names(ExtraData), ignore.case = TRUE)
names(ExtraData)<-gsub("-std()", "STD", names(ExtraData), ignore.case = TRUE)
names(ExtraData)<-gsub("-freq()", "Frequency", names(ExtraData), ignore.case = TRUE)
names(ExtraData)<-gsub("angle", "Angle", names(ExtraData))
names(ExtraData)<-gsub("gravity", "Gravity", names(ExtraData))

##step 7: #From the data set in step 6, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

TidyData<-group_by(ExtraData,Subject,Activity) 

TidyData<-summarise_all(TidyData,funs(mean))

write.table(TidyData, "TidyData.txt", row.name=FALSE)
