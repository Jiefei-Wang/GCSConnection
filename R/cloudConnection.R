gcs_connection <-function(description, open, 
                          encoding = "UTF8",
           credentials = getCredential(),
           project = getProjectName(),
           bucket = getBucketName()){
  stopifnot(
    is_scalar_character(credentials),
    is_scalar_character(project),
    is_scalar_character(bucket),
    is_scalar_character(description),
    is_scalar_character(open),
    is_scalar_character(encoding)
  )
  
  UTF8 = identical(encoding, "UTF8")
  autoOpen = TRUE
  
  if(open%in%c("r","rt")){
    isRead = TRUE
    isText = TRUE
  }
  if(open%in%c("w","wt")){
    isRead = FALSE
    isText = TRUE
  }
  if(open%in%c("rb")){
    isRead = TRUE
    isText = FALSE
  }
  if(open%in%c("wb")){
    isRead = FALSE
    isText = FALSE
  }
  getbucketConnection(credentials = credentials,
                         project = project,
                         bucket = bucket,
                         file = description,
                         isRead = isRead, istext = isText, 
                         UTF8 = UTF8, autoOpen = autoOpen)
}
    
           





listBuckets<- function(){
  initializeClientIfNot()
  C_get_bucket_names()
}

listFilesForBucket <- function(bucket = NULL){
  initializeClientIfNot()
  if(is.null(bucket))
    bucket = getBucketName(errorWhenNotSet = TRUE)
  fileList = C_get_file_names(bucket)
  fileList
}


