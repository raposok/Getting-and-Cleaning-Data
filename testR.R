# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Merge the training and the test sets to create one data set
## Read and then merge X
trainx <- read.table("train/X_train.txt")
testx <- read.table("test/X_test.txt")
X <- rbind(trainx, testx)
## Read and then merge S data
trains <- read.table("train/subject_train.txt")
tests <- read.table("test/subject_test.txt")
S <- rbind(trains, tests)
## Read and then merge Y
trainy <- read.table("train/y_train.txt")
testy <- read.table("test/y_test.txt")
Y <- rbind(trainy, testy)


# Extract only the measurements on the mean and standard deviation for each measurement
## Read features.txt to determine which measurements are required for mean and std
features <- read.table("features.txt")
## Extract mean and std
features_mean_std <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, features_mean_std]
names(X) <- features[features_mean_std, 2]
names(X) <- gsub("\\(|\\)", "", names(X))

# Use descriptive activity names rather than codes
## Read activity_labels.txt
activities <- read.table("activity_labels.txt")
## Match and replace codes with descriptions
activities[, 2] = gsub("_", "", as.character(activities[, 2]))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# Appropriately label the data set with descriptive variable names
names(S) <- "subject"
SYX <- cbind(S, Y, X)
write.table(SYX, "SYX_merged_clean_data.txt")
# head(SYX)
# tail(SYX)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
## Extract unique Subject
uniqueSubjects <- unique(S)[,1]
## Determine the length of Subject
numSubjects <- length(unique(S)[,1])
## Determine the length of Activities
numActivities <- length(activities[,1])
numCols <- dim(SYX)[2]
result <- SYX[1:(numSubjects*numActivities), ]
row <- 1
for (sub in 1:numSubjects) {
    for (act in 1:numActivities) {
        result[row, 1] = uniqueSubjects[sub]
        result[row, 2] = activities[act, 2]
        tmp <- SYX[SYX$subject==sub & SYX$activity==activities[act, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "result_data_set_with_the_averages.txt", row.names=FALSE)