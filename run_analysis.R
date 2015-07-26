# run_analysis.R
# Coursera Getting and Clearing data course script.

# Load funtcions source for some utilities.

source ("src/functions.R") 
library(dplyr)

# Initialize environment.

# clean:
CleanEnv()
# Write system specs into doc directory.
WriteSpecs()

# Initial values for variables

dataset.filename = "getdata_projectfiles_UCI HAR Dataset.zip"
dataset.url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# datasets path once uncompressed.

dataset.variables.labels="data/UCI HAR Dataset/features.txt"
dataset.activity.names="data/UCI HAR Dataset/activity_labels.txt"

dataset.train = "data/UCI HAR Dataset/train/X_train.txt"
dataset.train.labels = "data/UCI HAR Dataset/train/y_train.txt"
dataset.train.subjects = "data/UCI HAR Dataset/train/subject_train.txt"

dataset.test = "data/UCI HAR Dataset/test/X_test.txt"
dataset.test.labels = "data/UCI HAR Dataset/test/y_test.txt"
dataset.test.subjects = "data/UCI HAR Dataset/test/subject_test.txt"

# Check if data file exists and download it if don't.

DataExists (dataset.filename, dataset.url)

UnzipFile(dataset.filename)


##############################
### DATA PREPARATION
##############################

# Variable names array for all datasets:
DF.colNames = read.delim(dataset.variables.labels,
                         header=FALSE,
                         sep="",
                         stringsAsFactors=FALSE)
# Activity names
DF.activity.names = read.delim(dataset.activity.names,
                               header=FALSE,
                               sep="",
                               stringsAsFactors=FALSE)



## Train dataset.

train.activity = read.delim(dataset.train.labels,header=FALSE, sep="")
train.subjects = read.delim(dataset.train.subjects,header=FALSE, sep="")
colnames(train.subjects)= c("Subjects")


DF.train = read.delim(dataset.train,
                      header = FALSE,
                      sep = "",
                      dec = ".",
                      fill=FALSE,
                      col.names=DF.colNames[,2])

# merge activity and subjects into main DF. 
# id with activity name and add them to DF.train.

tmp = merge(x=DF.activity.names,y=train.activity)
colnames(tmp)= c( "id","Activity")

DF.train = cbind(DF.train, tmp[,2],train.subjects)
colnames(DF.train)[562]= c("Activity")

DF.train = mutate(DF.train,Data.Type="TRAIN")

## Test dataset.

test.activity = read.delim(dataset.test.labels,header=FALSE, sep="")
test.subjects = read.delim(dataset.test.subjects,header=FALSE, sep="")
colnames(train.subjects)= c("Subjects")

DF.test = read.delim(dataset.test,
                      header = FALSE,
                      sep = "",
                      dec = ".",
                      fill=FALSE,
                      col.names=DF.colNames[,2])

# merge activity and subjects into main DF. 
# id with activity name and add them to DF.train.

tmp = merge(x=DF.activity.names,y=test.activity)
colnames(tmp)= c( "id","Activity")

DF.test = cbind(DF.test, tmp[,2],test.subjects)
colnames(DF.test)[562]= c("Activity")
colnames(DF.test)[563]= c("Subjects")

DF.test = mutate(DF.test,Data.Type="TEST")
# Clean variables not used forward.
rm(list=c("DF.activity.names", "DF.colNames", 
          "test.activity","test.subjects",
          "tmp","train.activity",
          "train.subjects"))

##############################################################
# Merges the training and the test sets to create one data set.
##############################################################

DF.total = rbind(DF.train,DF.test)

# remove former datasets.

rm (list=c("DF.train","DF.test"))

#############################################################
# Extracts only the measurements on the mean and 
# standard deviation for each measurement. 
#############################################################

print (" Mean for all columns")
apply(DF.total[1:561],2,mean)

print (" Standard deviation for all columns")
apply(DF.total[1:561],2,sd)

#############################################################
# Uses descriptive activity names to name the activities 
# in the data set
#############################################################

# This action has already been done in de datapreparation phase
# for both datasets.
#
# tmp = merge(x=DF.activity.names,y=test.activity)
# colnames(tmp)= c( "id","Activity")

#############################################################
# Appropriately labels the data set with descriptive variable names. 
#############################################################

# This action has already been done in de datapreparation phase.
# for both datasets.
#
# colnames(train.subjects)= c("Subjects")

# DF.test = read.delim(dataset.test,
#                     header = FALSE,
#                     sep = "",
#                     dec = ".",
#                     fill=FALSE,
#                     col.names=DF.colNames[,2])


print ("Final Data set")
print (DF.total)


##############################################################
# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable 
# for each activity and each subject.
###############################################################

DF.new = group_by(DF.total,Activity,Subjects)

DF.new = summarise_each_(DF.new, funs(mean,sd), names(DF.new)[-(562:564)])


print (DF.new)

