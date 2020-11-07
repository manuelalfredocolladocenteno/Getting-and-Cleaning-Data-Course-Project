# Code book

## Features Feature Selection

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ tGravityAcc-XYZ tBodyAccJerk-XYZ tBodyGyro-XYZ tBodyGyroJerk-XYZ tBodyAccMag tGravityAccMag tBodyAccJerkMag tBodyGyroMag tBodyGyroJerkMag fBodyAcc-XYZ fBodyAccJerk-XYZ fBodyGyro-XYZ fBodyAccMag fBodyAccJerkMag fBodyGyroMag fBodyGyroJerkMag

The set of variables that were estimated from these signals are:

mean(): Mean value std(): Standard deviation mad(): Median absolute deviation max(): Largest value in array min(): Smallest value in array sma(): Signal magnitude area energy(): Energy measure. Sum of the squares divided by the number of values. iqr(): Interquartile range entropy(): Signal entropy arCoeff(): Autorregresion coefficients with Burg order equal to 4 correlation(): correlation coefficient between two signals maxInds(): index of the frequency component with largest magnitude meanFreq(): Weighted average of the frequency components to obtain a mean frequency skewness(): skewness of the frequency domain signal kurtosis(): kurtosis of the frequency domain signal bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window. angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean tBodyAccMean tBodyAccJerkMean tBodyGyroMean tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'

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
