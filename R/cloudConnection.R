getReadConnection<-function(fileName, project = NULL, bucket = NULL, credential = NULL){
  if(is.null(credential))
    credential = cloudSettings$credential
  
  if(is.null(project))
    project = cloudSettings$project
  
  if(is.null(bucket))
    bucket = cloudSettings$bucket
  
  if(is.null(credential))
    stop("Credential file is missing")
  if(is.null(project))
    stop("Project name is missing")
  if(is.null(bucket))
    stop("Bucket name is missing")
  
}


listBuckets<- function(){
  if(is.null(getProjectName()))
    stop("The project name is missing, please call `setProjectName` to set the project")
  C_get_bucket_names();
}

listFilesForBucket <- function(bucket = NULL){
  projectRecord = NULL
  if(!is.null(project)&&
     !is.null(cloudSettings$project)&&
     project!=cloudSettings$project)
    projectRecord = cloudSettings$projec
  if(is.null(project))
    project = cloudSettings$project
  if(is.null(project))
    stop("Project name is missing")
  
  fileList = C_get_file_names(bucket)
  
}


