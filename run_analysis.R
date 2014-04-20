DEFAULT_DIR <- "UCI HAR Dataset"
tidy_data <- function(directory=DEFAULT_DIR)
{
   cat(sprintf("Doing data cleaning in <%s> directory ...\n", directory))

      
   #read training set
   dt1 <- read_data("trial",  directory)
   dt2 <- read_data("trial2", directory)
   dt <- merge(dt1, dt2, by.x="subjects", by.y="subjects")
   dt
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
read_data <- function(datatype, dir=DEFAULT_DIR){
  # subject_<datatype>.txt: contains raw measurement data
  # X_<datatype>.txt      : 30 subjects
  # y_<datatype>.txt      : 6 activity types
  cat(sprintf("Working on <%s> dataset...\n", datatype))

  # read activity_labels.txt
  act_labels <- read_file("activity_labels.txt", dir)
  act_labels[,2] <- sapply(act[,2], tolower)
  
  y_file = paste0(datatype, "/y_", datatype, ".txt")
  y_dt <- read_file(y_file, dir)
  y_dt <- sapply(y_dt, function(x){ act_labels[x,2]})
  y_dt
  
  subjfile = paste0(datatype,"/subject_", datatype,".txt")
  subj_dt <- read_file(subjfile, dir)
  
  dt <- data.frame(subj_dt, y_dt)
  dt_names <- c("subjects", "activities")
  
  # read features.txt
  vec_features <- read_file("features.txt", dir)
  # get indexes of features with -mean() or -std()
  mean_std_idx <- grep("\\-(mean|std)\\(\\)", vec_features[,2])
  X_file = paste0(datatype, "/X_", datatype, ".txt")
  X_dt <- read_file(X_file, dir)
  X_dt
  
  for (idx in mean_std_idx) {
    dt_names <- c(dt_names, sub("-","_", vec_features[idx,2]))
    dt <- cbind(dt, X_dt[,idx])
  }
  names(dt) <- dt_names
  dt

}
