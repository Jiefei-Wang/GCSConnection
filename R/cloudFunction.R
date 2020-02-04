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
#' @examples
#' ## Open for reading the public Landsat data
#' ## on google cloud storage in text mode
#'
#' file <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
#' con <- gcs_connection(description = file, open = "rt")
#' readLines(con, n = 4L)
#' close(con)
#' @return A connection
#' @export
gcs_connection <- function(description,
                           open = "rb",
                           encoding = getOption("encoding"),
                           bucket = NULL) {
    stopifnot(
        is_scalar_character_or_null(bucket),
        is_scalar_character(description),
        is_scalar_character(open),
        is_scalar_character(encoding)
    )
    
    ## get the file name and bucket name from description
    file_info <- digest_path(description, bucket)
    file <- file_info$file
    bucket <- file_info$bucket
    description <- full_path(bucket, file)
    
    
    ## If unable to get the bucket name, use the default setting
    if (is.null(bucket))
        bucket <- googleCloudStorageR::gcs_get_global_bucket()
    
    token <- get_token()
    
    UTF8 <- identical(encoding, "UTF8")
    isText <- !grepl("b", open, fixed = TRUE)
    isRead <- grepl("r", open, fixed = TRUE)
    isWrite <- grepl("w", open, fixed = TRUE)
    
    if (isRead && isWrite) {
        stop("The connection must be in either read or write mode but not both.")
    }
    
    if (isRead) {
        bufferLength <- gcs_get_read_buff()
    } else{
        bufferLength <- gcs_get_write_buff()
    }
    
    
    autoOpen = TRUE
    
    get_bucket_connection(
        bucket = bucket,
        file = file,
        isRead = isRead,
        istext = isText,
        UTF8 = UTF8,
        autoOpen = autoOpen,
        buffLength = bufferLength,
        description = description,
        openMode = open
    )
}



#' copy files to and from buckets
#' 
#' The function supports moving files from bucket to bucket/ disk to bucket/ 
#' bucket to disk. Note that the existing destination file will be overwritten.
#' 
#' @param from,to Character. The path of the file. It can be either a file path
#' or a google URI.
#' @return No return value
#' @examples 
#' # Download a file to a disk
#' #gcs_cp("gs://bucket_name/file_name","file_path_on_disk")
#' @export
gcs_cp <- function(from, to){
    from_cloud <- is_google_uri(from)
    to_cloud <- is_google_uri(to)
    
    if(from_cloud&&to_cloud){
        from <-digest_path(from)
        to <- digest_path(to)
        copy_data_on_cloud(from, to)
        return(invisible())
    }
    
    if(from_cloud){
        from <-digest_path(from)
        download_data_to_disk(from$bucket, from$file, to)
        return(invisible())
    }
    
    if(to_cloud){
        to <- digest_path(to)
        upload_data_from_disk(from, to$bucket, to$file)
        return(invisible())
    }
    stop("Hey, I am a google cloud package. ",
         "Why do you use me to manage your disk file?")
}

#' List bucket/object
#' 
#' Get a list of objects in a bucket, or get a description of a file.
#' 
#' @param bucket Character(1), the name of the bucket
#' @param file character(1) or NULL, the name of the file
#' @param max_files integer or NULL, The maximum number of objects to 
#' return in a List request.
#' @example gcs_dir(bucket = "genomics-public-data")
#' @export
gcs_dir<-function(bucket,file=NULL, max_files = NULL){
    if(is.null(file)){
        .BucketClass(bucket,max_files)
    }else{
        .FileClass(get_file_meta(bucket,file))
    }
}
