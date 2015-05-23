# run_analysis.R
# Please see the README.md file for assumptions made for this script

   require("dplyr")
   require("reshape2")

# Important:  This script assumes that the UCI HAR Dataset has been downloaded to your
# working directory and unzipped.

# check for UCI folder in working directory

if(file.exists("./UCI HAR Dataset")){
  
  # read and load activity labels, feature labels, and all test/train data.
  
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",sep = " ")
    feature_labels <- read.table("./UCI HAR Dataset/features.txt", sep = " ")
  
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
    
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

  # apply appropriate column names to all data sets
  
    names(subject_test) <- "Subject"
    names(subject_train) <- "Subject"
    names(y_test) <- "Activity.ID"
    names(y_train) <- "Activity.ID"
    names(x_test) <- feature_labels$V2
    names(x_train) <- feature_labels$V2
    names(activity_labels) <- c("ID","Activity.Name")
  
  # merge subject/x/y data for test and train.
  
    test_data <- cbind(subject_test, y_test, x_test)
    train_data <- cbind(subject_train, y_train, x_train)
  
  # address column naming issue from original data
  
    valid_column_names <- make.names(names=names(test_data), unique=TRUE,allow_=TRUE)
    names(test_data) <- valid_column_names
    names(train_data) <- valid_column_names
  
  # add column to test/train data sets to identify them post merge
  
    test_data <- mutate(test_data, Set = "test")
    train_data <- mutate(train_data, Set= "train")
  
  # merge the test and train data sets
  
    master_data <- rbind(test_data,train_data)
  
  # add activity labels to master_data set
  
    master_data <- left_join(master_data,activity_labels, by = c("Activity.ID" = "ID"))
  
  # create a new data set from master_data that selects the columns Subject, Activity Name,
  # and variable columns that contain overall mean and standard deviation data.
  # Note:  see the README file for assumptions made in pulling out these variables.
  
    final_data <- select(master_data,Subject,Activity.Name,contains(".mean.", ignore.case=TRUE),contains(".std.",ignore.case=TRUE))
  
  # convert final_data to a meltable data frame
  
    final_data_df <- tbl_df(final_data)
  
  
  # melt data into a skinny data set where feature variables are a categorical vector
  
    skinny_data <- melt(final_data_df,id.vars=c("Subject","Activity.Name"),
                        measure.vars=c(3:68),value.name="value")
  
  # create a column that identifies features that contain 'mean' vs 'std'
  
    skinny_data$Val.Type <- ifelse(grepl("mean",skinny_data$variable),"mean","std") 
  
  # clean up variable column by eliminating 'mean', 'std', and stray periods, leaving a
  # descriptive feature name behind.
  
    skinny_data <- mutate(skinny_data, Var2 = gsub("mean","",variable,ignore.case=TRUE), 
                          Var3 = gsub("std","",Var2,ignore.case=TRUE), VarFinal = gsub("\\.","",Var3))
  
  # create the final tidy data set with avg for each variable by activity, subject, feature
  
    tidy_data_set <- dcast(skinny_data, Activity.Name + Subject + VarFinal ~ Val.Type,mean)
  
  # write tidy_data_set to a text file
  
    write.table(tidy_data_set, "./tidy_data_set.txt", row.names = FALSE)
  
    print("tidy_data_set.txt has been created in your working directory")

} else {
  stop("UCI HAR Dataset folder is not present.")
}
