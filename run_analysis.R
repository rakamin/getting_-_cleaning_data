###### Using relavant libraries for this program ####
library("dplyr")

#### Read all the 3 test and train datasets #######

###  TEST ###
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")

#### TRAIN ####
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")

#### cbinding to get a single test and a single train dataset ####

test_ds <- cbind(subject_test,Y_test,X_test)
train_ds <- cbind(subject_train,Y_train,X_train)

### rbinding test and train to a single dataset ###

combine_ds <- rbind(test_ds,train_ds)

### Adding names to the dataset ###
features <- read.table("./UCI HAR Dataset/features.txt")
name_ds <- as.vector(features[[2]])

names(combine_ds) <- c("Subject" , "Activity" , name_ds)

### Selecting columns with mean() & std() from name_ds  ###
mean_loc <- grep("mean()", names(combine_ds))
std_loc <- grep("std()", names(combine_ds))

ds_meansd <- combine_ds[,c(1,2,mean_loc,std_loc)]

### Getting Activity names & merging with dataset having mean & SD ###
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

ds_Act <- merge(ds_meansd,activity_labels, by.x = "Activity", by.y = "V1")

tidy1 <- ds_Act %>% tbl_df %>% select(-Activity) %>% rename(Activity_Name = V2) 

### Grouping and then Summarizing the dataset by taking the mean ###

tidy2 <- tidy1 %>% group_by(Activity_Name, Subject) %>%  summarise_each(funs(mean))

### Creating a text file as output ####
   write.table(tidy2,"tidy_ds.txt" , row.names = FALSE)


