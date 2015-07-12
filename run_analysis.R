# ########################################################################################
# Course: getdata-030
# Date:   07/13/2015
#
# Description: This R script generates a clean dataset from a raw data collected for the Activity Recognition 
# Experiment Using Smartphone Sensors.
#
# The following raw data is imported to a working directory in R.
#
# activity_labels  #Links the class labels with their activity name
# features         #List of all features
# 
# subject_test     #volunteers 1 - 30
# y_test           #activity 1-6
# X_test           #Test dataset
# 
# subject_train    #volunteers 1- 30
# y_train          #Activity 1 -6
# X_train          #Test data set
#
# For additional information take a look at the README file.
####################################################################################################
setwd("~/Documents/Data-Science/GettingCleaningData/ProgramAssign-1/workDir") #set working direcotry

library(plyr) # load the plyr lib
library(dplyr) #load the dplyr lib
# 
#features <- read.table("~/Documents/Data-Science/GettingCleaningData/ProgramAssign-1/UCI HAR Dataset/features.txt", quote="\"", comment.char="")

#
features$V1 <- NULL   # remove the extra column from features 

names(y_test)[1] <- "activity"
names(y_train)[1] <- "activity"

names(subject_test)[1] <- "volunteer"
names(subject_train)[1] <- "volunteer"

dim(features)
#[1] 561   1

dim(X_test)
#[1] 2947  561
dim(X_train)
#[1] 7352  561

#combine X_test & X_train, y_test & y_train, subject_test & subject_train
X_test_train <- rbind(X_test, X_train)
y_test_train <- rbind(y_test, y_train)
subject_test_train <- rbind(subject_test, subject_train)

dim(X_test_train)
# [1] 10299   561

#convert df to dt
class(X_test_train)
#[1] "data.frame"
X_test_train <- tbl_df(X_test_train) 
class(X_test_train)
#[1] "tbl_df"     "tbl"        "data.frame"

colnames(X_test_train) <- features$V2 #add the var labels for the combined dataset with a acolnames function

names(X_test_train)  #confirm the columnes var name has changed

df1 <- dplyr::bind_cols(subject_test_train, y_test_train, X_test_train) #bind the tree column 

dim(df1)
#[1] 10299   563

names(df1)
View(df1)

#match col 1 of activity_labels (*by.x) data frame "to" col activity of df1 (*by.y), create a column matching lables
df12 <- merge(df1, activity_labels, by.x='activity', by.y='V1', all.x=T)

df12 <- df12[,c(ncol(df12),1:(ncol(df12)-1))] # move the added column so it can be the first column

df12[,2]  <- NULL # remove a numeric only activity column

df12 = df12[,c(2,1,3:563)] #<- reorder columns - volunteer then activity then mean/std columns

# distill the values to to a single mean value for each each volunteer and each activity type 
df12Mean <- df12  %>% group_by(volunteer, activity)  %>% summarise_each(funs(mean))

df111 <- select(df12Mean, matches(".mean."))     #extract columns with only mean in ther column name
df112 <- select(df12Mean, matches(".std."))      #extract columns with only std in their column name
df113 <- select(df12Mean, activity) #extract column with activity in the column name


# Tidy table with 30 volunteers, each with 6 activity of 
# 3-axial linear acceleration and 3-axial angular velocity of smartphone accelerometer & gyroscope

dfFinal <- dplyr::bind_cols(df113, df111, df112) #bind the columns
dfFinal[,3] <- NULL # remove the redundunt volunteer column
dfFinal[,56] <- NULL # remove duplicate volunteer column

#Test
head(dfFinal, 20)
tail(dfFinal, 20)
View(dfFinal)
str(dfFinal)


#mydata <- read.table("tidyActvtyRecognSmartPhon.txt", header = TRUE) #save as txt
#write.csv(dfFinal, file="tidyActvtyRecognSmartPhon.csv") # save as csv



