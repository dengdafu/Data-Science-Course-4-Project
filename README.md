# Data-Science-Course-4-Project
This is the documentation for the Data Science Course 4 Project.  
  
## run_analysis.R 
The following is my code to perform the required task. SecondData is the required output data set.  
```
run_analysis <- function()  
{  
    ## Read in the data.  
    ## Data files are stored in "Project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset".  
    ## My working directory is "Project".  
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
```
  
## How the code works
### Firstly, data are read into the following variables (trimmed to keep the data of interest).
* features: stores all features, data from "feature.txt"
* colomns: stores all the features of interest (all means and all stds)
* activity_labels: stores all the labels for different activities (SITTING, STANDING, etc.), data from "activity_labels.txt"
* X_train_data: stores the data from the training data set, variables are given descriptive names, a column containing activities and a column containing subjects are also added to it, data from "X_train.txt"
* X_test_data: stores the data from the test data set, variables are given descriptive names, a column containing activities and a column containing subjects are also added to it, data from "X_test.txt"

### Secondly, the training set data and test set data are merged
```
X_merged_data <- rbind(X_train_data,X_test_data)
```

### Lastly, a new tidy data set is created by averaging each variable for each activity and each subject in X_merged_data
```
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
```
