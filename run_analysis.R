run_analysis <- function(){
# This function will create data files with the subject and activity files from the training and test data sets
# merge them together, create a subset of the merge data file containing columns with mean and std dev as required
# save them to a file

train_data <- get_data("train")  																    # read the training data set
test_data  <- get_data("test")   																    # read the test data set
merge_data <- rbind(train_data,test_data) 														    # merge the two data sets just read
colunms_selected <- c(562,563,grep("mean",names(merge_data),ignore.case="TRUE"),grep("std",names(merge_data),ignore.case="TRUE")) # Filter out all the columns with mean and std dev
summary_data <- merge_data[,columns_selected] 														    # create a subset of the merged data set with only the columns containing mean and std dev
summarized_data <-aggregate(summary_data, by=list(summary_data$Activity,summary_data$Subject),FUN=mean, na.rm=TRUE) 		    # summarized the mean and std dev by Subject and then Activity
write.table(summarized_data,row.names="TURE",col.names="TURE") 																	    # write the summarized data set out

}


get_data <- function(dir="train") {
# This function will read all the required data files need for the project into R

ori_dir = getwd() 														# save the current working directory
work_dir <- paste("D:/Downloads/R programming/Project/",dir,sep='') 						# create a path where the required files are contained
setwd(work_dir) 															# set the working directory to the path containing the files
data_X <- read.table(paste("X_",dir,".txt",sep=''),header=FALSE,sep='')						# read X_*****
subject <- read.table(paste("subject_",dir,".txt",sep=''),header=FALSE,sep='',col.names="Subject")	# read the subject file
activity <- read.table(paste("y_",dir,".txt",sep=''),header=FALSE,sep='',col.names="Activity")		# read the activity file
activity <- translate_activity_num(activity)										# replace the activity numbers in the activity file by activity lables
activity_features <- read.table("features.txt",header=FALSE,sep='',as.is=2)[,2]				# read the column names of the X_***** file 
colnames(data_X) <- activity_features											# put the column names into the X_***** file
data_X <- cbind(data_X,subject)												# add subject column to the X_*****
data_X <- cbind(data_X,activity)												# add activity column to the Y_*****
setwd(ori_dir)															# restore the original working directory
return(data_X)															# return the data frame created
}

translate_activity_num <- function(DF=data.frame()) {
# This function will translate the activity code in the activity file with the activity name

activity_list <- c("WALKING","WALKING UPSTAIRS","WALKING DOWNSTAIRS","SITTING","STANDING","LAYING")	# create a list of the activity labels
i <- 1																# initialize row counter
rows <- nrow(DF)															# get the number of rows in the activity dataframe
while(i <= rows) {														# Do while the counter is less than the number of rows in the dataframe
	DF[i,1] <- activity_list[as.numeric(DF[i,1])]									# replace the activity code in each row of the activity dataframe with the corresponding activity label
	i <- i+1															# increment the row counter
	}
return(DF)
}

