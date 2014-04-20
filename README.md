Introduction
------------
This script does the following tasks:

1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive activity names. 
5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

Usage
------
> source("run_analysis.R")
> dt <- tidy_data()

* function(directory="UCI HAR Dataset")
  If no directory is provided, it assumes "UCI HAR Dataset" directory is in the same working
  directory as "run_analysis.R".
* A file "tidydata.csv" will be created, containing the data in step 5 above.




