
#put the data together
library("tidyverse")

##set the workspace
setwd("F:/learningr/coursera_Getting and Cleaning Data/data/projectData/UCI HAR Dataset/")

ReadData <- function(paths){
    #dinfine an empty dataframe
    DataSets <- data.frame()
    for(path in paths){
        fileNames <- list.files(path, pattern = ".txt$", full.names = TRUE)
        data <- lapply(fileNames, function(x){
             read.table(x)})  #read data as list
        data <- as.data.frame(data) # list to data.frame
        #get data header
        #X_part
        headerFiles<- list.files(pattern = "features.txt$", full.names = TRUE)
        featuresLabels<- read.table(headerFiles,stringsAsFactors=FALSE)
        #y_part
        columns_y <- list(562,"activity_labels")
        #subject_part
        columns_subject  <- list(0,"subject")
        #rbind the three part header
        columns = rbind(columns_subject,featuresLabels,columns_y, stringsAsFactors=FALSE)
        # give data the colums name
        colnames(data) = columns$V2
        # rbind the data to Dataset
        DataSets <- rbind(DataSets,data, stringsAsFactors=FALSE)
    }
    DataSets
}

UCIData <- ReadData(paths = c("./train","./test"))

tail((UCIData))

dim(UCIData)

#grepl -find positions that meet with pattern
Mean_std_columns <- UCIData[,grepl("std()|mean()",colnames(UCIData))]
head(Mean_std_columns,2)

# as.factor(UCIData[, "activity_labels"]).
activityLabels <- read.table("./activity_labels.txt", stringsAsFactors=FALSE)
activityLabels

# an example: numeric_grade number to letter_grade letters 
#[https://stackoverflow.com/questions/28751879/how-to-map-a-column-through-a-dictionary-in-r]

# snape_gradebook_df<-data_frame(students=c("Harry", "Hermione", "Ron", "Neville", "Ginny", "Luna", "Draco", "Cho"),
#            numeric_grade=c(4,2,2,3,4,1,4,4))
# grade_map<-data_frame(numbers=c(1,2,3,4), letters=c("A", "B", "C", "D"),
#                     stringsAsFactors = FALSE)
# snape_gradebook_df['letter_grade']<-grade_map$letters[match(snape_gradebook_df$numeric_grade, grade_map$numbers)]
# snape_gradebook_df

# str(UCIData["activity_labels"]) #int
# str(activityLables["V1"]) #int
UCIData$activity_labels<- activityLabels$V2[match(UCIData$activity_labels,activityLabels$V1)]
head(UCIData,1)

#the body linear acceleration to obtain Jerk signals  (tBodyAccJerk-XYZ)
tBodyAccJerk <- function(str) gsub(pattern = "tBodyAcc" ,"TheBodyLinearAccelerationToObtainJerkSignals", str)
colNames <- sapply(colnames(UCIData),tBodyAccJerk)

#the angular velocity to obtain Jerk signals (tBodyGyro)
tBodyGyroJerk <- function(str) gsub(pattern = "tBodyGyro" ,"TheAngularVelocityToObtainJerksignals", str)
colNames <- sapply(colNames,tBodyGyroJerk)

#Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ
fBodyAcc  <- function(str) gsub(pattern = "fBodyAcc" ,"FastFourierTransformToObtainJerksignals", str)
colNames <- sapply(colNames,fBodyAcc)
colnames(UCIData) <- colNames
head(UCIData)

aggData <- aggregate(UCIData[,2:562], list(UCIData$subject,UCIData$activity_labels), mean)
colnames(aggData)[1:2] = c("subject_train","activity_labels")
tidyData  <-  aggData %>% arrange(subject_train)
head(tidyData,10)

dim(tidyData)
