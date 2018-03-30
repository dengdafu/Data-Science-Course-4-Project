# Data-Science-Course-4-Project
This is the repository for the Data Science Course 4 Project

run_analysis <- function()
{
  /## Read in the data.
  /## Data files are stored in "Project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset".
  /## My working directory is "Project".
  DataDir <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
  
  features <- read.table(paste0(DataDir,"/features.txt"))
  features <- as.character(features$V2)
  colomns <- grep("mean|std",features)
  
  activity_labels <- read.table(paste0(DataDir,"/activity_labels.txt"))
  activity_labels <- activity_labels$V2
  
  X_train_data <- read.table(paste0(DataDir,"/train/X_train.txt"))
  X_train_data <- X_train_data[,colomns]
  ## Add in the activity types
  y_train_data <- read.table(paste0(DataDir,"/train/y_train.txt"))
  y_train_data <- sapply(y_train_data, f <- function(x) activity_labels[x])
  X_train_data <- cbind(y_train_data, X_train_data)
  ## Add in the subjects
  subject_train_data <- read.table(paste0(DataDir,"/train/subject_train.txt"))
  X_train_data <- cbind(subject_train_data, X_train_data)
  ## Name the variables
  names(X_train_data) <- c("Subject","Activity",features[colomns])
  
  X_test_data <- read.table(paste0(DataDir,"/test/X_test.txt"))
  X_test_data <- X_test_data[,colomns]
  ## Add in the activity types
  y_test_data <- read.table(paste0(DataDir,"/test/y_test.txt"))
  y_test_data <- sapply(y_test_data, f <- function(x) activity_labels[x])
  X_test_data <- cbind(y_test_data, X_test_data)
  ## Add in the subjects
  subject_test_data <- read.table(paste0(DataDir,"/test/subject_test.txt"))
  X_test_data <- cbind(subject_test_data, X_test_data)
  ## Name the variables
  names(X_test_data) <- c("Subject","Activity",features[colomns])
  
  ## Merge the training and test sets
  X_merged_data <- rbind(X_train_data,X_test_data)
  
  ## From X_merged_data, create a second, independent tidy data set with
  ## the average of each variable for each activity and each subject
  subjects <- sort(unique(X_merged_data$Subject))
  activities <- as.character(unique(X_merged_data$Activity))
  allvariables <- names(X_merged_data) ## include Subject & Activity
  
  ## initialize the second data set
  SecondData <- data.frame(matrix(ncol = length(allvariables), 
                                  nrow = length(subjects) * length(activities)))
  names(SecondData) <- allvariables
  
  i <- 1
  while (i <= length(subjects))
  {
    subject_data <- X_merged_data[X_merged_data$Subject==subjects[i],]
    j <- 1
    while (j <= length(activities))
    {
      SecondData[(i-1)*length(activities)+j,1] <- subjects[i]
      SecondData[(i-1)*length(activities)+j,2] <- activities[j]
      subject_activity_data <- subject_data[subject_data$Activity == activities[j],]
      means <- colMeans(subject_activity_data[,3:length(allvariables)])
      SecondData[(i-1)*j+j, 3:length(allvariables)] <- means
      j <- j + 1
    }
    i <- i + 1
  }
  
  ## return the new data set
  SecondData
}
