# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# 1. Merge the training and the test sets to create one data set.

# set working directory
setwd("./UCI HAR Dataset 2")

# Read in the data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assign colnames
colnames(activityType) = c("activityId","activityType")
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; # assign the colnames of xTrain based on the second column of table - features
colnames(yTrain)        = "activityId";

# Merge the training dataset - yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Read in test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Merge the test dateset - subjectTest, xTest, yTest
testData = cbind(yTest,subjectTest,xTest)

# Merge the training and test data
finalData = rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used to select the desired mean and stddev() columns
colNames = colnames(finalData)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# Create a logicalVector that contains T values for the ID, mean() & stddev() columns and F for others
# Keep relevant variables
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
table(logicalVector) # To test how many columns are relevant
finalData = finalData[logicalVector==TRUE] # Finalize data, keep the relevant variables

# 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData with the activityType to include the activityType name
finalData = merge(finalData,activityType,by="activityId",all.x=TRUE)

# Update colNames
colNames = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
    colNames[i] = gsub("\\()","",colNames[i]) # delete colnames contains "\\()"
    colNames[i] = gsub("-std$","StdDev",colNames[i]) # change "-std$" to "StdDev" in colNames
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassign the new descriptive column names to the finalData set
colnames(finalData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType = finalData[,names(finalData) != 'activityType']

# Summarize the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c("activityId","subjectId")],by=list(activityId=finalDataNoActivityType$activityId,subjectId=finalDataNoActivityType$subjectId),mean)

# Merge the tidyData with activityType to include descriptive activity names
tidyData = merge(tidyData,activityType,by="activityId",all.x=TRUE)

# Export tidy data
write.table(tidyData,"./tidyData.txt",row.names=TRUE,sep="\t")





























