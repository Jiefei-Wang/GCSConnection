#' Connection to google cloud storage
#' 
#' This function creates an R connection to a file on google cloud storage.
#' 
#' @param description character string. The name of the file that you want to connect to.
#' It can be either the file name or a full path to the file.
#' @param open character string. A description of how to open the connection.
#' See `?connections` for possible values.
#' @param encoding character string. The character encoding. Currently only support either `Unicode`
#' or `UTF8`
#' @param credentials character string. A file path to the JSON credentials. If not supplied, the default
#' value in `gcs_get_cloud_auth()` will be used. If the credentials is an empty string(AKA ""), an anonymous 
#' credentials will be supplied.
#' @param bucket character string. The name of the bucket that the file is located in. If not supplied, 
#' value in `gcs_get_global_bucket()` will be used. If a full path to the file is provided in `description`,
#' this parameter will be ignored.
#' 
#' @examples 
#' ## Connect to the Landsat data on google cloud storage
#' ## This is a public dataset so there is no need to provide
#' ## credentials.
#' file <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
#' con <- gcs_connection(description = file, open = "r", credentials = "")
#' readLines(con, n = 1L)
#' close(con)
#' 
#' @export
gcs_connection <-function(description, open, 
                          encoding = getOption("encoding"),
           credentials = NULL,
           bucket = NULL){
  stopifnot(
    is_scalar_character_or_null(credentials),
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
  
  
  
  ## If unable to get the bucket name, use the default setting
  if(is.null(bucket)) 
    bucket <- gcs_get_global_bucket()
  
  if(is.null(credentials))
    credentials <- get_token()
  
  
  UTF8 <- identical(encoding, "UTF8")
  isText <- !grepl("b",open,fixed = TRUE)
  isPublic <- grepl("p",open,fixed = TRUE)
  isRead <- grepl("r",open,fixed = TRUE)
  isWrite <- grepl("w",open,fixed = TRUE)
  
  if(isRead&&isWrite){
    stop("The connection must be in either read or write mode but not both.")
  }
  
  if(isRead){
    bufferLength <- gcs_get_input_stream_buff()
  }else{
    bufferLength <- gcs_get_output_stream_buff()
  }
  
  
    
  
  
  autoOpen = TRUE
  project <- ""
  
  get_bucket_connection(credentials = credentials,
                         project = project,
                         bucket = bucket,
                         file = file,
                         isRead = isRead,  isPublic = isPublic,
                        istext = isText, UTF8 = UTF8, 
                        autoOpen = autoOpen,
                         buffLength = bufferLength,
                        description = description,openMode = open)
}
    






