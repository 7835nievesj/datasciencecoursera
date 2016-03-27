# Set working directory
setwd("~/Documents/Class_2016/Data.Scientist.Coursera/GitHub.Coursera/
      Getting and Cleaning Data/UCI HAR Dataset")

# PART 1. Merges the training and the test sets to create one data set.

# Read TRAIN files
X_train <- read.csv("./train/X_train.txt", 
                    header = FALSE, 
                    sep = "", 
                    colClasses = "numeric",
                    comment.char = "")

y_train <- read.csv("./train/y_train.txt", header = FALSE, sep = "")
subject_train <- read.csv("./train/subject_train.txt", header = FALSE, sep = "")

# Prepare X_train data frame
train_df <- cbind(subject_train,y_train,X_train)

# Read TEST files
X_test <- read.csv("./test/X_test.txt", 
                   header = FALSE, 
                   sep = "", 
                   colClasses = "numeric",
                   comment.char = "")

y_test <- read.csv("./test/y_test.txt", header = FALSE, sep = "")
subject_test <- read.csv("./test/subject_test.txt", header = FALSE, sep = "")

# Prepare X_test data frame
test_df <- cbind(subject_test,y_test,X_test)

# Merge data sets 
trainTest.df1 <- rbind(train_df,test_df)

# PART 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# use features.txt to identify variable names
features <- read.csv("features.txt", header = FALSE, sep = "")[,2]
varNames <- as.character(features)

# Identify the variable names that include "mean" and "std" (standard deviation)
meanSTD.col <- grep("mean|std", varNames) + 2
# moves values 2 spaces to count for y and subject 

# Extract columns with mean and std values from merged data frame
trainTest.df2 <- trainTest.df1[ , c(1,2,meanSTD.col)]

# PART 3.  Uses descriptive activity names to name the activities in the data set

# Read activity labels
activitylabels <- read.table("./activity_labels.txt",
                             col.names=c("activityId","activityName"))
# Set activityID as factor  
trainTest.df2[,2] <- factor(trainTest.df2[,2])

# Set levels for activityID with activityName
activityNames <- activitylabels$activityName
levels(trainTest.df2[,2]) <- activityNames

# PART 4.  Appropriately labels the data set with descriptive variable names.
meanSTD.names <- grep("mean|std", varNames, value = TRUE)
colnames(trainTest.df2) <- c("subjectID","activityName", meanSTD.names)

# PART 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
melt.df <- melt(trainTest.df2, id = c("subjectID","activityName")) 
tidy.df <- dcast(melt.df, subjectID + activityName ~ variable,fun.aggregate = mean)
write.table(tidy.df, file = "tidy_df.txt", row.names = FALSE)

