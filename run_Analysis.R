run_Analysis <- function(){
    library(reshape2)
    
    filepath <- "UCI HAR Dataset"
    
    # Load activity labels + features
    activityLabels <- read.table(file.path(filepath,"activity_labels.txt"))
    activityLabels[,2] <- as.character(activityLabels[,2])
    features <- read.table(file.path(filepath,"features.txt"))
    features[,2] <- as.character(features[,2])
    
    # Extract only the data on mean and standard deviation
    featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
    featuresWanted.names <- features[featuresWanted,2]
    featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
    featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
    featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)
    
    # Load the datasets
    trainfilepath <- file.path(filepath,"train")
    trainData <- read.table(file.path(trainfilepath,"X_train.txt"))[featuresWanted]
    trainActivities <- read.table(file.path(trainfilepath,"Y_train.txt"))
    trainSubjects <- read.table(file.path(trainfilepath,"subject_train.txt"))
    trainData <- cbind(trainSubjects, trainActivities, trainData)
    
    testfilepath <- file.path(filepath, "test")
    testData <- read.table(file.path(testfilepath,"X_test.txt"))[featuresWanted]
    testActivities <- read.table(file.path(testfilepath,"Y_test.txt"))
    testSubjects <- read.table(file.path(testfilepath,"subject_test.txt"))
    testData <- cbind(testSubjects, testActivities, testData)
    
    # merge datasets and add labels
    allData <- rbind(trainData, testData)
    colnames(allData) <- c("subject", "activity", featuresWanted.names)
    
    # turn activities & subjects into factors
    allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
    allData$subject <- as.factor(allData$subject)
    
    allData.melted <- melt(allData, id = c("subject", "activity"))
    allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)
    
    write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
}