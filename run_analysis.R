#% run_analysis.R

#%//-----[Organize paths]-------

MainDirectoryPath <- "/Users/juanantonioteran/Desktop/My\ R\ Working\ Directory/UCI\ HAR\ Dataset"
SubdirectoryTest <- file.path(MainDirectoryPath, "test")
SubdirectoryTrain <- file.path(MainDirectoryPath, "train")

FilePathXTest <- file.path(SubdirectoryTest, "X_test.txt")
FilePathXTrain <- file.path(SubdirectoryTrain, "X_train.txt")

FilePathYTest <- file.path(SubdirectoryTest, "Y_test.txt")
FilePathYTrain <- file.path(SubdirectoryTrain, "Y_train.txt")

FilePathSubjectTest <- file.path(SubdirectoryTest, "Subject_test.txt")
FilePathSubjectTrain <- file.path(SubdirectoryTrain, "Subject_train.txt")

FilePathFeatures <- file.path(MainDirectoryPath, "features.txt")
FilePathActivityLabels <- file.path(MainDirectoryPath, "Activity_labels.txt")

#%//-----[Load Data]-------

FeaturesTest  <- read.table(FilePathXTest, header = FALSE)
FeaturesTrain <- read.table(FilePathXTrain, header = FALSE)

ActivityTest  <- read.table(FilePathYTest, header = FALSE)
ActivityTrain <- read.table(FilePathYTrain, header = FALSE)

SubjectTest  <- read.table(FilePathSubjectTest, header = FALSE)
SubjectTrain <- read.table(FilePathSubjectTrain, header = FALSE)

FeaturesFile <- read.table(FilePathFeatures, header = FALSE)
ActivityLabels <- read.table(FilePathActivityLabels, header = FALSE)

#%——————————————————————————————————————————————————————————————————————————————————————————————————
#% 1. Merges the training and the test sets to create one data set.
#%——————————————————————————————————————————————————————————————————————————————————————————————————

#%//-----[1.1: Merge "train" and "test" sets by rows]-------

DataFeatures<- rbind(FeaturesTrain, FeaturesTest)
DataSubject <- rbind(SubjectTrain, SubjectTest)
DataActivity<- rbind(ActivityTrain, ActivityTest)

#%//-----[1.2: Change Column Names (assign proper names to variables in each file)]-------

names(DataFeatures) <- FeaturesFile$V2
names(DataSubject) <- c("Subject")
names(DataActivity) <- c("Activity")

#%//-----[1.3: Merge columns to create data frame consisting of all data]-------
AllDataMerged <- cbind(DataFeatures, DataSubject, DataActivity)

#%——————————————————————————————————————————————————————————————————————————————————————————————————
#% 2. Extracts only the measurements on the mean and standard deviation for each variable.
#%——————————————————————————————————————————————————————————————————————————————————————————————————

AllDataMergedColumnNames <- colnames(AllDataMerged)
SearchString1 <- "mean|std|subject|activity|Subject|Activity"
SDAndMeanData <- AllDataMerged[ ,grepl(SearchString1, AllDataMergedColumnNames)]

#%——————————————————————————————————————————————————————————————————————————————————————————————————
#% 3. Uses descriptive activity names to name the activities in the data set
#%——————————————————————————————————————————————————————————————————————————————————————————————————
colnames(ActivityLabels)<-c("Activity","ActivityType")

library(plyr)

SDAndMeanData <- join(SDAndMeanData, ActivityLabels, by = "Activity", match = "first")

SDAndMeanData <- SDAndMeanData[ ,-1]
#%takes away the first column (Activity) which is not very descriptive

#%——————————————————————————————————————————————————————————————————————————————————————————————————
#% 4. Appropriately labels the data set with descriptive variable names.
#%——————————————————————————————————————————————————————————————————————————————————————————————————

names(SDAndMeanData)<-gsub("^t", "Time", names(SDAndMeanData))
names(SDAndMeanData)<-gsub("^f", "Frequency", names(SDAndMeanData))
names(SDAndMeanData)<-gsub("Acc", "Accelerometer", names(SDAndMeanData))
names(SDAndMeanData)<-gsub("Gyro", "Gyroscope", names(SDAndMeanData))
names(SDAndMeanData)<-gsub("Mag", "Magnitude", names(SDAndMeanData))
names(SDAndMeanData)<-gsub("BodyBody", "Body", names(SDAndMeanData))

#%——————————————————————————————————————————————————————————————————————————————————————————————————
#% 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#%——————————————————————————————————————————————————————————————————————————————————————————————————

SecondDataSet <- aggregate(. ~Subject + ActivityType, SDAndMeanData, mean)
#% Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.

SecondDataSet <- SecondDataSet[order(SecondDataSet$Subject,SecondDataSet$ActivityType), ]
FilePathFinalDataSet <- file.path("./", "FinalDataSet.txt")

write.table(SecondDataSet, file = FilePathFinalDataSet, row.name=FALSE)