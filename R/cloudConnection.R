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