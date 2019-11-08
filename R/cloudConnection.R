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

getInternalConnection <- function(fileName, bucket, text, UTF8, isRead, open){
  validateArgument(fileName, bucket, text, UTF8)
  if(is.null(bucket))
    bucket = getBucketName(errorWhenNotSet = TRUE)
  
  getbucketConnectionCPP(credentials = getCredential(errorWhenNotSet = TRUE),
                         project = getProjectName(errorWhenNotSet = TRUE),
                         bucket = bucket,
                         file = fileName,
                         canRead = isRead, canWrite = !isRead,
                         text = text, UTF8 = UTF8, open=open
  )
}


getBucketConnection <- function(fileName, bucket = NULL, 
                                open = "r", UTF8 =FALSE){
  open <- TRUE
  if(open%in%c("r","rt")){
    return(getInternalConnection(fileName, bucket, text = TRUE , UTF8 = UTF8, 
                          isRead = TRUE , open= open))
  }
  if(open%in%c("w","wt")){
    return(getInternalConnection(fileName, bucket, text = TRUE , UTF8 = UTF8, 
                          isRead = FALSE , open= open))
  }
  if(open%in%c("rb")){
    return(getInternalConnection(fileName, bucket, text = FALSE , UTF8 = FALSE, 
                          isRead = TRUE , open= open))
  }
  if(open%in%c("wb")){
    return(getInternalConnection(fileName, bucket, text = FALSE , UTF8 = FALSE, 
                          isRead = FALSE , open= open))
  }
  
  stop("unsupported open option")
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


