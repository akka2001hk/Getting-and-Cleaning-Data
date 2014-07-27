get_data <- function(dir="train") {
ori_dir = getwd()
work_dir <- paste("D:/Downloads/R programming/Project/",dir,sep='')
setwd(work_dir)
data_X <- read.table(paste("X_",dir,".txt",sep=''),header=FALSE,sep='')
subject <- read.table(paste("subject_",dir,".txt",sep=''),header=FALSE,sep='',col.names="Subject")
activity <- read.table(paste("y_",dir,".txt",sep=''),header=FALSE,sep='',col.names="Activity")
activity <- translate_activity_num(activity)
activity_features <- read.table("features.txt",header=FALSE,sep='',as.is=2)[,2]
colnames(data_X) <- activity_features
data_X <- cbind(data_X,subject)
data_X <- cbind(data_X,activity)
setwd(ori_dir)
return(data_X)
}

translate_activity_num <- function(DF=data.frame()) {
activity_list <- c("WALKING","WALKING UPSTAIRS","WALKING DOWNSTAIRS","SITTING","STANDING","LAYING")
i <- 1
rows <- nrow(DF)
while(i <= rows) {
	DF[i,1] <- activity_list[as.numeric(DF[i,1])]
	i <- i+1
	}
return(DF)
}
