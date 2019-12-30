#' Connection to google cloud storage
#' 
#' This function creates an R connection to a file on google cloud storage.
#' A service account credentials is required for accessing private data, the 
#' credentials can be set via `gcs_cloud_auth`
#' 
#' @param description character string. The name of the file that you want to connect to.
#' It can be either the file name or a full path to the file.
#' @param open character string. A description of how to open the connection.
#' See details for possible values. If not specified, the default value will be
#' "rb" if a credential is set or "rbp" if not.
#' @param encoding character string. The encoding of the input/output stream of a connection. 
#' Currently the parameter `encoding` should be either `native.enc` or `UTF8`. see `?connections` for more detail.
#' @param bucket character string. The name of the bucket that the file is located in. If not supplied, 
#' value in `gcs_get_global_bucket()` will be used. If a full path to the file is provided in `description`,
#' this parameter will be ignored.
#' 
#' @details 
#' Possible values for the argument `open` are the combination of the following characters:
#' 
#' "r" or "w" : read or write mode. The GCS connection cannot be in both read and write modes.
#' 
#' "t" or "b" : text or binary mode. If not specified, the default is text mode.
#' 
#' "p": Public data access mode. If specified, Credentials is not required. The public data
#' access mode only works with read connections.
#' 
#' 
#' @examples 
#' ## Open for reading the public Landsat data 
#' ## on google cloud storage in text mode
#' 
#' file <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
#' con <- gcs_connection(description = file, open = "rtp")
#' readLines(con, n = 4L)
#' close(con)
#' 
#' @export
gcs_connection <-function(description, open = "", 
                          encoding = getOption("encoding"),
                          bucket = NULL){
  stopifnot(
    is_scalar_character_or_null(bucket),
    is_scalar_character(description),
    is_scalar_character(open),
    is_scalar_character(encoding)
  )
  
  ## get the file name and bucket name from description
  file_info <- digest_path(description,bucket)
  file <- file_info$file
  bucket <- file_info$bucket
  description <- full_path(bucket,file)
  
  
  if(open == ""){
    if(!is.null(package_settings[["credentials"]])){
      open <- "rb"
    }else{
      open <- "rbp"
    }
  }
  ## If unable to get the bucket name, use the default setting
  if(is.null(bucket)) 
    bucket <- gcs_get_global_bucket()
  
  token <- get_token()
  
  UTF8 <- identical(encoding, "UTF8")
  isText <- !grepl("b",open,fixed = TRUE)
  isPublic <- grepl("p",open,fixed = TRUE)
  isRead <- grepl("r",open,fixed = TRUE)
  isWrite <- grepl("w",open,fixed = TRUE)
  
  if(isRead&&isWrite){
    stop("The connection must be in either read or write mode but not both.")
  }
  if(isPublic&&isWrite){
    stop("Write to a public data is not supported")
  }
  
  
  if(isRead){
    bufferLength <- gcs_get_read_buff()
  }else{
    bufferLength <- gcs_get_write_buff()
  }
  
  
  autoOpen = TRUE
  
  get_bucket_connection(bucket = bucket,
                         file = file,
                         isRead = isRead,  isPublic = isPublic,
                        istext = isText, UTF8 = UTF8, 
                        autoOpen = autoOpen,
                         buffLength = bufferLength,
                        description = description,openMode = open)
}
    






