DEFAULT_DIR <- "UCI HAR Dataset"
MEAN_STD_REGEX <- "\\-(mean|std)\\(\\)"

tidy_data <- function(directory=DEFAULT_DIR)
{
   cat(sprintf("Doing data cleaning in <%s> directory ...\n", directory))

      
   #read dataset keeping only measurements with "-mean()" and "-std()".
   if(0){ # 1 for debug
     dt1 <- read_dataset("trial",  directory)
     dt2 <- read_dataset("trial2", directory)
   } else {
     dt1 <- read_dataset("train", directory)
     dt2 <- read_dataset("test", directory)
   }
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
  #mean_std_idx <- grep("\\-(mean|std)\\(\\)", features[,2])
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
