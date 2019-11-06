validateArgument <- function(fileName, bucket, text, UTF8){
  if(!is.character(fileName))
    stop("The argument 'fileName' must be a string")
  if(length(fileName)!=1)
    stop("You can only specify one file name in the argument 'fileName'")
  if(!is.logical(text))
    stop("The argument 'text' must be a logical value")
  if(!is.logical(UTF8))
    stop("The argument 'UTF8' must be a logical value")
  if(!is.null(bucket)&&!is.character(bucket))
    stop("The argument 'bucket' must be a string")
}


getReadConnection<-function(fileName, bucket = NULL, text = TRUE, UTF8 =FALSE){
  validateArgument(fileName, bucket, text, UTF8)
  if(is.null(bucket))
    bucket = getBucketName(errorWhenNotSet = TRUE)
  
  getbucketConnectionCPP(credentials = getCredential(errorWhenNotSet = TRUE),
                         project = getProjectName(errorWhenNotSet = TRUE),
                         bucket = bucket,
                         file = fileName,
                         canRead = TRUE, canWrite = FALSE,
                         text = text, UTF8 = UTF8
                         )
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
}


