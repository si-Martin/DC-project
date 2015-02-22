#Script caters for the automatic merging of the test and training dataset
#Coursera - Getting and Cleaning Data course
# prepared by si-Martin, Monday February 9 2015

# See the following paper for the data origin and data collectors
#[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
#     Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
#     International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

#merge train & test data
#1. Subjects
if (!exists("SubjTest")) SubjTest <- read.table("./test/subject_test.txt", header=FALSE)
if (!exists("SubjTrain")) SubjTrain <- read.table("./train/subject_train.txt", header=FALSE)
Subjects <- rbind(SubjTest, SubjTrain)
colnames(Subjects) <- c("Subjects")

#2. First I merge the X_train.txt and X_test.txt from the main folder
if (!exists("XdataTest")) XdataTest <- read.table("./test/X_test.txt", header=FALSE)
if (!exists("XdataTrain")) XdataTrain <- read.table("./train/X_train.txt", header=FALSE)
Xdata <- rbind(XdataTest, XdataTrain) # the actual fusion of data

#2. Here I merge the Y_train.txt and Y_test.txt from the main folder
if (!exists("YdataTest")) YdataTest <- read.table("./test/Y_test.txt", header=FALSE)
if (!exists("YdataTrain")) YdataTrain <- read.table("./train/Y_train.txt", header=FALSE)
Ydata <- rbind(YdataTest, YdataTrain) # the actual fusion of data

#3. Here I merge the data in the Inertial Signals folder (9 files - 3 set per  2 files X,y,z)
#It has been done for the sake of the exercise. I don't use the data for the calculations.
# Set 1 - body acc : body_accx_test.txt
#read the test data
if (!exists("SigBaXtest")) SigBaXtest <- read.table("./test/Inertial Signals/body_acc_x_test.txt", header=FALSE)
if (!exists("SigBaYtest")) SigBaYtest <- read.table("./test/Inertial Signals/body_acc_y_test.txt", header=FALSE)
if (!exists("SigBaZtest")) SigBaZtest <- read.table("./test/Inertial Signals/body_acc_z_test.txt", header=FALSE)
# read the train data
if (!exists("SigBaXtrain")) SigBaXtrain <- read.table("./train/Inertial Signals/body_acc_x_train.txt", header=FALSE)
if (!exists("SigBaYtrain")) SigBaYtrain <- read.table("./train/Inertial Signals/body_acc_y_train.txt", header=FALSE)
if (!exists("SigBaZtrain")) SigBaZtrain <- read.table("./train/Inertial Signals/body_acc_z_train.txt", header=FALSE)
# merge the data in a single variable
SigBaX <- rbind(SigBaXtest,SigBaXtrain)
SigBaY <- rbind(SigBaYtest,SigBaYtrain)
SigBaZ <- rbind(SigBaZtest,SigBaZtrain)

# Set 2 - body gyro : body_gyro_x_test.txt
#read the test data
if (!exists("SigGyXtest")) SigGyXtest <- read.table("./test/Inertial Signals/body_gyro_x_test.txt", header=FALSE)
if (!exists("SigGyYtest")) SigGyYtest <- read.table("./test/Inertial Signals/body_gyro_y_test.txt", header=FALSE)
if (!exists("SigGyZtest")) SigGyZtest <- read.table("./test/Inertial Signals/body_gyro_z_test.txt", header=FALSE)
# read the train data
if (!exists("SigGyXtrain")) SigGyXtrain <- read.table("./train/Inertial Signals/body_gyro_x_train.txt", header=FALSE)
if (!exists("SigGyYtrain")) SigGyYtrain <- read.table("./train/Inertial Signals/body_gyro_y_train.txt", header=FALSE)
if (!exists("SigGyZtrain")) SigGyZtrain <- read.table("./train/Inertial Signals/body_gyro_z_train.txt", header=FALSE)
# merge the data in a single variable
SigGyX <- rbind(SigGyXtest,SigGyXtrain)
SigGyY <- rbind(SigGyYtest,SigGyYtrain)
SigGyZ <- rbind(SigGyZtest,SigGyZtrain)

# Set 3 - total acc : total_acc_x_test.txt
#read the test data
if (!exists("SigTaXtest")) SigTaXtest <- read.table("./test/Inertial Signals/total_acc_x_test.txt", header=FALSE)
if (!exists("SigTaYtest")) SigTaYtest <- read.table("./test/Inertial Signals/total_acc_y_test.txt", header=FALSE)
if (!exists("SigTaZtest")) SigTaZtest <- read.table("./test/Inertial Signals/total_acc_z_test.txt", header=FALSE)
# read the train data
if (!exists("SigTaXtrain")) SigTaXtrain <- read.table("./train/Inertial Signals/total_acc_x_train.txt", header=FALSE)
if (!exists("SigTaYtrain")) SigTaYtrain <- read.table("./train/Inertial Signals/total_acc_y_train.txt", header=FALSE)
if (!exists("SigTaZtrain")) SigTaZtrain <- read.table("./train/Inertial Signals/total_acc_z_train.txt", header=FALSE)
# merge the data in a single variable
SigTaX <- rbind(SigTaXtest,SigTaXtrain)
SigTaY <- rbind(SigTaYtest,SigTaYtrain)
SigTaZ <- rbind(SigTaZtest,SigTaZtrain)


#3 Use descriptive activity names to name the activities in the data set
if (!exists("ActLab")) ActLab <- read.table("activity_labels.txt", header=FALSE)
#mergedDataset <- merge(dataset1, dataset2, by.x="X", by.y="CountryCode")
mergedDataY <- merge(Ydata, ActLab, by.x="V1", by.y="V1")
colnames(mergedDataY) <- c("ActivityID","ActivityLabel")

#4) Appropriately labels the data set with descriptive variable names.
if (!exists("Feat")) Feat <- read.table("features.txt", header=FALSE)
colnames(Xdata) <- Feat$V2

#5) From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject

Cdataset <- cbind(Subjects, mergedDataY, Xdata)
#Cdataset <- na.omit(Cdataset)
CdatasetAvgAll <- aggregate(Cdataset[-3], by = list(Cdataset$Subjects, Cdataset$ActivityID), FUN=mean)
CdatasetAvgSubj <- aggregate(Cdataset[-3], by = list(Cdataset$Subjects), FUN=mean)
CdatasetAvgActiv <- aggregate(Cdataset[-3], by = list(Cdataset$ActivityID), FUN=mean)

#recording results in the csv files
write.table(CdatasetAvgAll, "results-ALL.csv", col.names=TRUE, row.name=FALSE,  sep=",")
write.table(CdatasetAvgSubj, "results-Subj.csv", col.names=TRUE, row.name=FALSE, sep=",")
write.table(CdatasetAvgActiv, "results-Activ.csv", col.names=TRUE, row.name=FALSE , sep=",")



