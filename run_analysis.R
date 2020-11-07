# Needed packages
library(plyr)
library(dplyr)

# Download file
temp<-tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = temp)
unzip(temp)

# Read X
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X <- join(X_train, X_test, type = "full")

# Read y
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y <- bind_rows(y_train, y_test)

# Read subject
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject <- bind_rows(subject_train, subject_test)
names(subject) <- "subject"

# Read features
features <- read.table("UCI HAR Dataset/features.txt")
# Only mean or std features
featuresX <- X[,grep("mean()|std()", features$V2)]

# Descriptive activity names to name the activities in the data set
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
set_activity <- function(data, label){
  result <- data
  for(activity in 1:6){
    result <- replace(result[1], result[1]==activity, label[activity,2])
    
  }
  
  result
}

y_labeled <- set_activity(y, activity_labels)
names(y_labeled) <- "activity"


feats <- features[grep("mean()|std()", features$V2),][[2]]

# Descriptive variable names
feats <- gsub('^f', "Frequency", feats)
feats <- gsub('^t', "Time", feats)
feats <- gsub('Acc', 'Acceleration', feats)
feats <- gsub('Gyro', 'Gyroscope', feats)
feats <- gsub('Mag', 'Magnitude', feats)
feats <- gsub('BodyBody', 'Body', feats)

names(featuresX) <- feats

df <- bind_cols(subject, y_labeled, featuresX)

# Independent tidy data set with the average of each variable 
# for each activity and each subject
tidy_dataset <- df %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(tidy_dataset, "tidy_dataset.txt", row.name=FALSE)
