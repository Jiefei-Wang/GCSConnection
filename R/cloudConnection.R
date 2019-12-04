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
  description <- file_info$file
  bucket <- file_info$bucket
  
  ## If unable to get the bucket name, use the default setting
  if(is.null(bucket)) 
    bucket <- gcs_get_global_bucket()
  
  if(is.null(credentials))
    credentials <- gcs_get_cloud_auth_internal(useAnonymous = TRUE)
  
  
  UTF8 <- identical(encoding, "UTF8")
  
  if(open%in%c("r","rt")){
    isRead <- TRUE
    isText <- TRUE
  }
  if(open%in%c("w","wt")){
    isRead <- FALSE
    isText <- TRUE
  }
  if(open%in%c("rb")){
    isRead <- TRUE
    isText <- FALSE
  }
  if(open%in%c("wb")){
    isRead <- FALSE
    isText <- FALSE
  }
  autoOpen = TRUE
  project <- ""
  get_bucket_connection(credentials = credentials,
                         project = project,
                         bucket = bucket,
                         file = description,
                         isRead = isRead, istext = isText, 
                         UTF8 = UTF8, autoOpen = autoOpen,
                         readBuffLength = gcs_get_input_stream_buff())
}
    






