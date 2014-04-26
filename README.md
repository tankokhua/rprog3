## Introduction

This script performs the following steps below to generate a tidy data set from raw data provided:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Description of Raw Data
* The raw data has the file hierarchy as shown below:
* There are 2 main datasets represented by the "train" and "test" folders.
  * The "Inertial signals" folders in "test" and "train" datasets are not used.
  * "X_(train|test).txt" contains the raw measurements.


```{r}
.
|-- README.txt
|-- activity_labels.txt              (contains the mapping to 6 activities)
|-- features.txt
|-- features_info.txt
|-- test
|   |-- Inertial Signals
|   |   |-- body_acc_x_test.txt
|   |   |-- body_acc_y_test.txt
|   |   |-- body_acc_z_test.txt
|   |   |-- body_gyro_x_test.txt
|   |   |-- body_gyro_y_test.txt
|   |   |-- body_gyro_z_test.txt
|   |   |-- total_acc_x_test.txt
|   |   |-- total_acc_y_test.txt
|   |   `-- total_acc_z_test.txt
|   |-- X_test.txt                   (contains 2947 rows)
|   |-- subject_test.txt
|   `-- y_test.txt
`-- train
    |-- Inertial Signals
    |   |-- body_acc_x_train.txt
    |   |-- body_acc_y_train.txt
    |   |-- body_acc_z_train.txt
    |   |-- body_gyro_x_train.txt
    |   |-- body_gyro_y_train.txt
    |   |-- body_gyro_z_train.txt
    |   |-- total_acc_x_train.txt
    |   |-- total_acc_y_train.txt
    |   `-- total_acc_z_train.txt
    |-- X_train.txt                  (contains 7352 rows)
    |-- subject_train.txt
    `-- y_train.txt

4 directories, 28 files
```          

## run_analysis.R
### Output
```{r}
> source("./run_analysis.R")
> dt <- tidy_data()
Doing data cleaning in <UCI HAR Dataset> directory ...
Working on <train> dataset ...
Working on <test> dataset ...
[1] "Merging <train> and <test> datasets."
Writing to tidydata.txt.
> 
```
### Notes:
* Function call:  tidy_data(directory=DEFAULT_DIR)
* If no directory is provided, it assumes "UCI HAR Dataset" directory is in the same working
  directory as "run_analysis.R".
* A file "tidydata.csv" will be created, containing the data in step 5 above.
* The regular expression used to extract measurements for the mean and standard deviation is 
  as below, which matches any measurements with **"-mean()"** or **"-std()"**. Note: **"-meanFreq()"** is
  not included.

```{r}
         MEAN_STD_REGEX <- "\\-(mean|std)\\(\\)"
```
* You can change the regular expression MEAN_STD_REGEX to filter the measurements you wanted.
* The output of the tidy data is saved in CSV format as "tidydata.txt".
* Refer to CodeBook.md for the description of the "tidydata.txt".

### Pseudocode
1. Read "train" dataset using "read_dataset()"
   * Read "activity_labels.txt" using "read_file()"
     * Change the labels to lower case.
   * Read "y_train.txt" using "read_file()"
     * Replace the numerical value with the corresponding activity label. ie. '1' change to 'walking', etc.
   * Read "subject_train.txt" using "read_file()"
   * Create dataframe combining the "subjects" and the "activities"
   * Read "X_test.txt" using "read_file()"
     * Read "features.txt" using "read_file()" which to get the indices and column names for measurements in "X_test.txt"
     * Use "grep" to obtain the measurements that matches the "\\-(mean|std)\\(\\)" expression, and add them to the dataframe (containing "subjects" and "activities") created earlier.
   * Return the dataframe.
2. Repeat step 1 for "test" dataset
3. Combined the datasets in Steps 1 and 2.
4. Melt the combined dataset using "subjects" and "activities" as id, and the measurements as variables.
5. Dcast the "melt" dataset with formula "subjects+activities~variable", and "mean" to obtain the final dataset.

### Contents

```{r}
DEFAULT_DIR <- "UCI HAR Dataset"
MEAN_STD_REGEX <- "\\-(mean|std)\\(\\)"

tidy_data <- function(directory=DEFAULT_DIR)
{
   cat(sprintf("Doing data cleaning in <%s> directory ...\n", directory))

      
   #read dataset keeping only measurements with "-mean()" and "-std()".
   dt1 <- read_dataset("train", directory)
   dt2 <- read_dataset("test", directory)

   # combine "train" and "test" datasets
   print('Merging <train> and <test> datasets.')
   dt <- merge(dt1, dt2, all=TRUE)
    
   meas_names <- names(dt)[3:length(names(dt))]
   library(reshape2)
   melt_dt <- melt(dt, id=c("subjects", "activities"), measure.vars=meas_names)
   tidy <- dcast(melt_dt, subjects+activities~variable, mean)
   
   # write to file
   cat(sprintf("Writing to %s/tidydata.txt.", getwd()))
   write.csv(tidy, file="tidydata.txt", row.names=FALSE)
   tidy
}

read_file <- function(filename, dir=DEFAULT_DIR) {
  fullpath <- paste(dir, "/", filename, sep="")
  if(file.exists(fullpath)) {
    dt <- read.table(fullpath)
  } else {
    stop(paste(filename, "not found."))
  }
  dt
}

read_dataset <- function(datatype, dir=DEFAULT_DIR){
  # subject_<datatype>.txt: contains raw measurement data
  # X_<datatype>.txt      : 30 subjects
  # y_<datatype>.txt      : 6 activity types
  cat(sprintf("Working on <%s> dataset ...\n", datatype))

  # read activity_labels.txt
  act_labels <- read_file("activity_labels.txt", dir)
  act_labels[,2] <- sapply(act_labels[,2], tolower)
  
  y_file = paste0(datatype, "/y_", datatype, ".txt")
  y_dt <- read_file(y_file, dir)
  y_dt <- sapply(y_dt, function(x){ act_labels[x,2]})
  y_dt
  
  subjfile = paste0(datatype,"/subject_", datatype,".txt")
  subj_dt <- read_file(subjfile, dir)
  
  dt <- data.frame(subj_dt, y_dt)
  dt_names <- c("subjects", "activities")
  
  # read features.txt
  features <- read_file("features.txt", dir)
  # get indexes of features with -mean() or -std()
  mean_std_idx <- grep(MEAN_STD_REGEX, features[,2])
  X_file = paste0(datatype, "/X_", datatype, ".txt")
  X_dt <- read_file(X_file, dir)
  X_dt
  for (idx in mean_std_idx) {
    dt_names <- c(dt_names, as.character(features[idx,2]))
    dt <- cbind(dt, X_dt[,idx])
  }
  names(dt) <- dt_names
  dt
}
```



