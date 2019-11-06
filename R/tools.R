cloudSettings <- list(
  credential = NULL,
  project = NULL,
  bucket = NULL,
  initialized = FALSE
)


getCredential<-function(errorWhenNotSet = FALSE){
  name = cloudSettings$credential
  if(errorWhenNotSet&&is.null(name)){
    stop("The credential file is not set, please call `setCredential` to set the credential")
  }
  name
}
setCredential <- function(fileName){
  if(!is.null(getCredential())&&getCredential()==fileName)
    return()
  cloudSettings$credential <- fileName
  C_set_credential(fileName)
  cloudSettings$initialized = FALSE
}


getProjectName <- function(errorWhenNotSet = FALSE){
  name = cloudSettings$project
  if(errorWhenNotSet&&is.null(name)){
    stop("The project name is not set, please call `setProjectName` to set the project")
  }
  name
}
setProjectName <- function(projectName){
  if(!is.null(getProjectName())&&getProjectName()==fileName)
    return()
  cloudSettings$project <- projectName
  C_set_project(projectName);
  cloudSettings$initialized = FALSE
}


getBucketName <- function(){
  cloudSettings$bucket
}
setBucketName <- function(bucketName){
  cloudSettings$bucket <- bucketName
}

initializeClientIfNot <- function(){
  if(!cloudSettings$initialized){
    
  }
}


