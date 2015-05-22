library(plyr)
library(dplyr)
library(data.table)


### loading the training and test set as well as ther activity_labels####

##Loading test set
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

##loading train set

x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")


### rename V1 labels

subject_test <- rename(subject_test, Subject = V1)
y_test <- rename(y_test, Y = V1)

subject_train <- rename(subject_train, Subject = V1)
y_train <- rename(y_train, Y = V1)

### combining and merging the data to a (new) complete dataframe

test_data <- cbind(subject_test, y_test, x_test)
train_data <- cbind(subject_train,y_train,x_train)

data <- rbind(test_data,train_data)


### adding the descriptive activity names to name the activities in the dataset

activity_labels <- read.table("activity_labels.txt")
activity_labels <- rename(activity_labels, activity = V2, id = V1)

data <- merge(data, activity_labels, by.x = "Y", by.y = "id" , all = TRUE)


### adding labels for V1 to V561 

new_names <- read.table("features.txt")
new_names <- new_names[,2]

old_names <- select(data, -activity,-Y, -Subject)
old_names <- colnames(old_names)

setnames(data, old = old_names, new = as.vector(new_names))


### extract only the columns which are for the standard deviation and the mean


##creating a list of the column-names with "mean" inside
mean1 <- NULL
for (i in 1:length(names(data))) {
        mean <- grep("mean",names(data[i]), value = TRUE)
        mean1 <- c(mean1, mean)
} 

# the mean1 -list includes also the meanFreq, which needs to be removed from the list: 

list2 <- grep("meanFreq", mean1)
mean1 <- mean1[-list2]


##creating a list of the column-names with "std" inside
std1 <- NULL
for (i in 1:length(names(data))) {
        std <- grep("std",names(data[i]), value = TRUE)
        std1 <- c(std1, std)
} 

## concatenating both list

col_names <- c(std1,mean1)

#### remove all columns that do not refer to the mean/std. Except of subject, activity and Y

data <- data[,names(data) %in% c(mean1,std1,"Y","Subject", "activity")]

### Change order, so that the activity is shown in column 2 (optional)

data <- select(data,Y, activity,Subject, 1:69)

### Create a 2. dataset with the average of each variable for each activity and each subject

new_data <- (data %>% group_by(Subject,activity) 
             %>% summarise_each(funs(mean))
             %>% arrange(desc(Subject),Y))

## remove the Y-Value so that the dataframe becomes tidier. (each Y refers to an activity)

new_data <- select(new_data,-Y)

##Saving the tidy dataframe as tidy_data.txt

write.table(new_data,"tidy_data.txt", row.name = FALSE)


#### Thank you for reading, i hope that it was understandable! :-)