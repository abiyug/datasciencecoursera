####################################################################################################################
# Script Name: run-analysis.R                                                                                      #
# Date: July 25, 2015                                                                                              #
# Course: getdata-030                                                                                              #
# Description: This script will download a ziped untidy dataset and will tidy and store it in csv format.          #
#              The script output can be verified using the steps at the end of the script.                         #
#              For more information on variables and measurments take a look at the README & CookBook documents.   #
#                                                                                                                  #
####################################################################################################################

# Create a 'data' directory, if it doesn't exist already, download, unzip and save the files from the given URL.
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/SamsungGalaxy.zip", method = "curl")

unzip("./data/SamsungGalaxy.zip") # unzip the downloaded file

#copy the "test" raw data to the active directory
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

#copy the "train" raw data to the active directory
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

#copy the lable and feature files to working directory
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

######################## Add packages ###############################################################

library(plyr)
library(dplyr)
library(data.table)

###################### Modify labels fortables ######################################################
features$V1 <- NULL   # remove the extra column from features 

names(y_test)[1] <- "activity" # change the default var name for y_test df
names(y_train)[1] <- "activity"# change the default var name for y_test df

names(subject_test)[1] <- "volunteer"  # change the default var name for subject_test df
names(subject_train)[1] <- "volunteer" # change the default var name for subject_test df

########################Combine the two datasets from test & train #################################

X_test_train <- rbind(X_test, X_train)
y_test_train <- rbind(y_test, y_train)
subject_test_train <- rbind(subject_test, subject_train)

colnames(X_test_train) <- features$V2 #add the var labels for the combined dataset with the features table

df1 <- dplyr::bind_cols(subject_test_train, y_test_train, X_test_train) #bind activity, subject and X tables together by column 

df1 <- tbl_df(df1)  #Convert the combined data frame to data table to use plyr/dplyr packages

############### Convert activity_lable descriptoin to lower case ####################################
actLvl <- sapply(activity_labels, tolower) # convert to lower case
actLvl <- as.data.table(actLvl) # the above step convert the table into matrix. This step brings it back to table format
actLvl$V1 <- as.numeric(as.character(actLvl$V1)) # Changes the column to numeric

df12 <- df1 # make a copy of the data table for  backup

df12$activity <- with(actLvl, V2[match(df12$activity, V1)]) #replace activity lable from number to description

#write.csv(df12, file = "Combined-raw-data-train-test.csv") # copy the combined data as backup

duplicated(colnames(df12)) #Verify dplicate column existance - False is none True is yes
df12NoneDup <- df12[,!duplicated(colnames(df12))]#remove the duplicated columns

############################ Group the data by volunteer and activity and distill the data to single mean value per activity
df12Mean <- df12NoneDup  %>% group_by(volunteer, activity)  %>%  summarise_each(funs(mean))

############################ Extract the volunteer, activity, mean and standard diviation measures Columns only####
ActvRecgnData <- select(df12Mean, volunteer, activity, matches(".mean."), matches(".std."))

###################### Changing variable names to a more readable format##############################
names(ActvRecgnData) <- gsub("BodyAcc-mean()", "BAm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("GravityAcc-mean()",  "GAm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerk-mean()" , "BAJm" , names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyro-mean()" , "BGm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroJerk-mean()" , "BGJm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("GravityAccMag-mean()" , "GAMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroMag-mean()" , "BGMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAcc-meanFreq()", "BAmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerk-meanFreq()",  "BAJmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyro-meanFreq()" , "BGmF" , names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccMag-mean()" , "BAMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyAccJerkMag-mean()" , "BBAJMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroJerkMag-mean()" , "BBGJMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroJerkMag-mean()" , "BBGJMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerkMag-mean()" , "BAJMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroJerkMag-mean()" , "BGJMm", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccMag-meanFreq()" , "BAMmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyAccJerkMag-meanFreq()" , "BBAJMmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroMag-meanFreq()" , "BBGMmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroJerkMag-meanFreq()" , "BBGJMmF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAcc-std()", "BAs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("GravityAcc-std()",  "GAs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerk-std()" , "BAJs" , names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyro-std()" , "BGs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroJerk-std()" , "BGJs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("GravityAccMag-std()" , "GAMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroMag-std()" , "BGMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAcc-stdFreq()", "BAsF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerk-stdFreq()",  "BAJsF", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyro-stdFreq()" , "BGsF" , names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccMag-std()" , "BAMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyAccJerkMag-std()" , "BBAJMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroJerkMag-std()" , "BBGJMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyBodyGyroJerkMag-std()" , "BBGJMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyAccJerkMag-std()" , "BAJMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("BodyGyroJerkMag-std()" , "BGJMs", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(tBodyAccMean,gravity)" , "atBAMg", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(tBodyAccJerkMean),gravityMean)" , "atBAJMgM", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(tBodyGyroMean,gravityMean)" , "atBGMgM", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(tBodyGyroJerkMean,gravityMean)" , "atGJMgM", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(X,gravityMean)" , "aXgM", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(Y,gravityMean)" , "aYgM", names(ActvRecgnData), fixed = TRUE)
names(ActvRecgnData) <- gsub("angle(Z,gravityMean)" , "aZgM", names(ActvRecgnData), fixed = TRUE)

names(ActvRecgnData) <- gsub("-" , "", names(ActvRecgnData), fixed = TRUE) #remove the - from the variables

############### Saving the table#######################################################################
write.csv(ActvReconData, file = "ActivityRecogni-fnl.csv") #the final "tidy data" saved...

write.table(ActvRecgnData, file="ActivityRecogn-fnl.txt", row.names = FALSE) #save as text

####### The size of the clean data is shrunk by 99.7%!!!

############ Confirm/test the output file############################################################
# > dim(df12)
# [1] 10299   563    <------------Before tidy data table dimension 563 columns and 10,299 rows
# 
# > object.size(df12) <-----------Before tiday data table size in bytes 46.4 MB
# 46412664 bytes
# 
# End data set size is as follows:
# > dim(ActvRecgnData)
# [1] 180  88     <--------After tidy data table dimension to 88 Columns and 180 rows
# > 
# > object.size(ActvRecgnData)
# 142872 bytes    <--------After tidy data table size in bytes 0.14MB
# 
# > (object.size(ActvRecgnData) / object.size(df12)) * 100
# 0.307829776804021 bytes  <--- 0.31%  
#
# > length(grep("mean", names(ActvRecgnData))) <--After
# [1] 46
# > length(grep("std", names(ActvRecgnData))) <--After
# [1] 33
