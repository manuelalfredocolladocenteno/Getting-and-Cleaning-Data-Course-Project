# Code book

## Files

Below there  is a description of the files that contained the data necessary for this course project and how each file was read into R:

features.txt
a text file containing 561 'features' that describe each observation of the data. Each feature represents a column.

activity_labels.txt
a text file containing the different activities and their code.

xtest.txt
a text file that contains data on each features for 2947 observations of the test subset of volunteers.

ytest.txt
a text file that contains the corresponding activity ID for each of the 2947 observations of the test subset of volunteers.

subject_test.txt
a text file that contains the corresponding participant ID for each of the 2947 observations of the test subset of volunteers.

xtrain.txt
a text file that contains data on each features for 7352 observations of the train subset of volunteers.

ytrain.txt
a text file that contains the corresponding activity ID for each of the 7352 observations of the train subset of volunteers.

subject_train.txt
a text file that contains the corresponding participant ID for each of the 7352 observations of the train subset of volunteers.

## Variables

Below there is a concise summary of all the variables used in the run_analysis.R script:

features: 561 x 2 provides the names of all of the features (columns) in the xtrain/xtest data sets
activitie_labels: 6 x 2 provides the correlation between activity number and activity name in the test data subset
X_test: 2947 x 561 provides values for the 561 features across 2947 observations in the test data subset
X_train: 7352 x 561 provides values for the 561 features across 7352 observations in the train data subset
X: 10299 x 561 (merges X_test and X_train)
y_test: 2947 x 1 provides the activity for each of the 2947 recorded observations in the test data subset
y_train: 7352 x 1 provides the activity for each of the 7352 recorded observations in the train data subset
y: 10299 x 1 (merges y_test and y_train)
subject_test: 2947 x 1 provides the subject (i.e. ParticipantID) for each of the 2947 recorded observations
subject_train: 7352 x 1 provides the subject (i.e. ParticipantID) for each of the 7352 recorded observations in the train data subset
subject: 10299 x 1 (merges subject_test and subject_train)
features: 531 x 2 data from features.txt
featuresX: 10299 x 79 data from X but with just features as columns
y_labeled: 10299 x 1 data from 'y' but substituting codes to activities
feats: 79 char vector with changed column names
df: 10299 x 81 data from the subject, featuresX and y_labeled
tidy_dataset: 180 x 81 the average value of features that only reference mean & std, grouped by activity and subject

## Process

1. We add the needed libraries.
2. We download the data set in a temporary file.
3. We start reading the datasets X, y, and subject

```
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
```

4. Then we select only the mean or std features

```
# Read features
features <- read.table("UCI HAR Dataset/features.txt")
# Only mean or std features
featuresX <- X[,grep("mean()|std()", features$V2)]
```

5. After that we set descriptive activity names to the activity column.

```
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
```

6. Then we set descriptive variable names and merge all the datasets.

```
feats <- features[grep("mean()|std()", features$V2),][[2]]

# Descriptive variable names
feats <- gsub('^f', "frecuency", feats)
feats <- gsub('^t', "time", feats)
feats <- gsub('Acc', 'Accelerometer', feats)
feats <- gsub('Gyro', 'Gyroscope', feats)
feats <- gsub('Mag', 'Magnitude', feats)
feats <- gsub('BodyBody', 'Body', feats)

names(featuresX) <- feats

df <- bind_cols(subject, y_labeled, featuresX)

```

7. Finally, we create the data set that contains the mean summarised of each variable by subject and activity.

```
# Independent tidy data set with the average of each variable 
# for each activity and each subject
tidy_dataset <- df %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(tidy_dataset, "tidy_dataset.txt", row.name=FALSE)

```

