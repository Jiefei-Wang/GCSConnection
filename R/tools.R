cloudSettings <- list(
  credential = NULL,
  project = NULL,
  bucket = NULL
)


getCredential<-function(){
  cloudSettings$credential
}
setCredential <- function(fileName){
  cloudSettings$credential <- fileName
}


getProjectName <- function(){
  cloudSettings$project
}
setProjectName <- function(projectName){
  cloudSettings$project <- projectName
}


getBucketName <- function(){
  cloudSettings$bucket
}
setBucketName <- function(bucketName){
  cloudSettings$bucket <- bucketName
}




