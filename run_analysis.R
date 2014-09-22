getXYData <- function()
{
	## Get x data
	df_xtrain <- read.table("X_train.txt")
	df_xtest <- read.table("X_test.txt")		

	## Get y data
	df_ytrain <- read.table("y_train.txt")
	df_ytest <- read.table("y_test.txt")	

	## Get subject data
	df_testSubject <- read.table("subject_test.txt")
	df_trainSubject <- read.table("subject_train.txt")

	## Get features
	df_features <- read.table("features.txt")

	## Add features to x data
	names(df_xtrain) <- df_features$V2		
	names(df_xtest) <- df_features$V2

	## Rename column name to a meaninigfull one. 
	names(df_ytrain) <- "Activity"
	names(df_ytest) <- "Activity"

	# Rename column name to a meaninigfull one. 
	names(df_trainSubject) <- "SubjectID"
	names(df_testSubject) <- "SubjectID"

	## Now combine subject, x data, and y data into 1 data set
	df_allTrain <- cbind(df_trainSubject, df_xtrain, df_ytrain)
	df_allTest <- cbind(df_testSubject, df_xtest, df_ytest)

	## Now merge all train and test data into 1 big data set
	df_combined <- rbind(df_allTrain, df_allTest)

	return (df_combined)

}

set_activityName <- function (df)
{	
	df$Activity[df$Activity == 1] = "WALKING"
    	df$Activity[df$Activity == 2] = "WALKING_UPSTAIRS"
    	df$Activity[df$Activity == 3] = "WALKING_DOWNSTAIRS"

    	df$Activity[df$Activity == 4] = "SITTING"
    	df$Activity[df$Activity == 5] = "STANDING"
    	df$Activity[df$Activity == 6] = "LAYING"
    
	return (df)
}


main <- function()
{
	## Get all necessary data (x data, y data, subject data) and merged them into 1 dataset
	df_All <- getXYData()

	## Uses descriptive activity names to name the activities in the data set
	df_All <- set_activityName(df_All)

	## Get columns with mean/std and add SubjectId and Activity to the data set
	x <- c(names(df_All))
	meanstdCol <- grep("mean\\(\\)|std\\(\\)", x)
	df_ALL <- df_All[,c(meanstdCol, 1, 563)]	
	
	##creates a second, independent tidy data set with the average of each variable for each activity and 
	tidy <- aggregate(df_All, by=list(activity = df_All$Activity, subject=df_All$SubjectID), mean)
	write.table(tidy, "ProjectTidy.txt", sep="\t")

}

