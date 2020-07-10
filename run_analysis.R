run_analysis <- function(){
        
        library(stats)
        library(utils)
        
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        if (!file.exists("dataset.zip")){download.file(fileUrl,destfile="dataset.zip",method="curl")}
        if (!file.exists("UCI HAR Dataset")) {unzip("dataset.zip")}
        
        subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt",header = FALSE)
        subjectTest  <- read.table("UCI HAR Dataset/test/subject_test.txt",header = FALSE)
        
        activityTrain <- read.table("UCI HAR Dataset/train/Y_train.txt",header = FALSE)
        activityTest  <- read.table("UCI HAR Dataset/test/Y_test.txt",header = FALSE)
        activityLabel <- read.table("UCI HAR Dataset/activity_labels.txt",header = FALSE)
        activityLabel[,2] <- as.character(activityLabel[,2])
        
        featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt",header = FALSE)
        featuresTest  <- read.table("UCI HAR Dataset/test/X_test.txt",header = FALSE)
        featureName <- read.table("UCI HAR Dataset/features.txt",header = FALSE)
        featureName[,2] <- as.character(featureName[,2])
        
        dataTrain <- cbind(subjectTrain,activityTrain,featuresTrain)
        dataTest <- cbind(subjectTest,activityTest,featuresTest)
        data <- rbind(dataTrain,dataTest)

        colnames(data)[1] <- "subject"
        colnames(data)[2] <- "activity"
        colnames(data)[3:ncol(data)] <- featureName[,2]
        
        featureSelect <- featureName[,2][grep("(mean|std)\\(\\)", featureName[,2])]
        data <- subset(data,select=c("subject", "activity", as.character(featureSelect)))
        
        data[,2] <- factor(data[,2], levels = activityLabel[,1], labels = activityLabel[,2])
        data[,1] <- as.factor(data[,1])
        
        names(data)<-gsub("std()", "SD", names(data))
        names(data)<-gsub("mean()", "MEAN", names(data))
        names(data)<-gsub("^t", "time", names(data))
        names(data)<-gsub("^f", "frequency", names(data))
        names(data)<-gsub("Acc", "Accelerometer", names(data))
        names(data)<-gsub("Gyro", "Gyroscope", names(data))
        names(data)<-gsub("Mag", "Magnitude", names(data))
        names(data)<-gsub("BodyBody", "Body", names(data))
        
        data2<-aggregate(. ~subject + activity, data, mean)
        data2<-data2[order(data2$subject,data2$activity),]
        write.table(data2, file = "tidy.txt",row.names=FALSE)
        write.csv(data2, file = "tidy.csv",row.names=FALSE)
}