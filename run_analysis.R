run_Analysis  <- function() {
  
            require("data.table")
            X_test <- fread("test\\X_test.txt")
            y_test <- fread("test\\y_test.txt")
            
            X_train <- fread("train\\X_train.txt")
            y_train <- fread("train\\y_train.txt")
            
            
            features <- fread("features.txt")
            featureNames <- features[, features$V2]
            #tweaking some column names
            featureNames <- gsub("BodyBody", "Body", featureNames)
            featuresasChar <- as.character(featureNames)
            #setting names to the 2 data sets
            X_test = setNames(X_test, featuresasChar)
            X_train = setNames(X_train, featuresasChar)
            
            #merging the 2 data sets
            mergedXTestAndXTrain  <- rbind(X_test, X_train)
            #head(mergedXTestAndXTrain)
            
            #Indices of the columns that match regex mean or std
            namesofactivites <- names(mergedXTestAndXTrain)
            subnamesofact <- namesofactivites[grep("mean\\(\\)|std\\(\\)", namesofactivites)]
            submergedXTestAndXTrain <- subset(mergedXTestAndXTrain,select=subnamesofact)
           
            
            subjectTest <- fread("test\\subject_test.txt")
            #dim(subjectTest)  2947    1
           
            subjectTrain <- fread("train\\subject_train.txt")
            #dim(subjectTrain) 7352    1
            
            #merging activity name with subject Id, 2 column in this dataset, this data table will include all the activities each subject performed 
            mergeYTrainWithSubjectTrain <- cbind(y_train, subjectTrain)
            mergeYTestWithSubjectTest <- cbind(y_test, subjectTest)
            #adding names to the 2 columns
            mergeYTestWithSubjectTest <- setNames(mergeYTestWithSubjectTest, c("activityName", "subjectId"))
            mergeYTrainWithSubjectTrain <- setNames(mergeYTrainWithSubjectTrain, c("activityName", "subjectId"))
            #merging activities with subjectIds from both train and test
            mergyYTestAndTrainBySubjectID <- rbind(mergeYTrainWithSubjectTrain, mergeYTestWithSubjectTest)
            #merging activities with subjectIds and with the subset of test according to mean or std
            allmerged <- cbind(mergyYTestAndTrainBySubjectID, submergedXTestAndXTrain)
            #translating integer for activity id to a string mapped to the integer
            allmerged$activityName <- factor(allmerged$activityName, labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))
            
            #calculating average for each subject for each activity
            tinydata <- aggregate(. ~subjectId + activityName, allmerged, mean)
            
            #writing tinydata.txt
            write.table(tinydata, "tinydata.txt", row.names = FALSE, sep = "\t")
            
}
