package_settings <- new.env()
package_settings[["credentials"]] <- ""
package_settings[["input_buff_len"]] <- 1024*1024
package_settings[["output_buff_len"]] <- 1024 *1024

digest_path <- function(description, bucket){
  if(length(grep("gs://",description,fixed=T)) == 1){
    bucket <- strsplit(description,"/")[[1]][3]
    file <- sub(paste0("gs://",bucket,"/"),"",description)
  }else{
    file <- description
  }
  list(file = file,bucket = bucket)
}

is_scalar_character <- function( x ) {
  is.character(x) && length(x) == 1
}
is_scalar_character_or_null <- function( x ) {
  is.null(x) || is_scalar_character(x)
}
is_scalar_logical <- function( x ) {
  is.logical(x) && length(x) == 1
}

#' Get/Set google credential file
#' 
#' Authenticate with Google Cloud Storage. You can download the JSON credential 
#' file from Google Gloud Platform. The credentials can be set globally by the 
#' evironment variable `GOOGLE_APPLICATION_CREDENTIALS` or `GCS_AUTH_FILE`.
#' 
#' @param creds character string. A JSON file that can be used to authenticate
#' Google Cloud Storage.
#' @details 
#' When the package is loaded, it first searchs the credential file from the enviroment 
#' variable `GOOGLE_APPLICATION_CREDENTIALS`. If the credentials is not found, the environment variable
#' `GCS_AUTH_FILE` will be used intead. If both variables are not specified. Users need to specify the
#' credentials by calling `gcs_cloud_auth` function.
#' 
#' @rdname authentication
#' @examples 
#' ## Get the default credential file
#' gcs_get_cloud_auth()
#' @export
gcs_cloud_auth <- function(creds){
  package_settings[["credentials"]] <- creds
  # if(!is.null(creds)){
  #   gcs_auth(creds)
  # }
}

#' @rdname authentication
#' @export
gcs_get_cloud_auth <- function(){
  gcs_get_cloud_auth_internal()
}
gcs_get_cloud_auth_internal <- function(useAnonymous = FALSE){
  creds <- package_settings[["credentials"]]
  if(useAnonymous && is.null(creds)){
    return("")
  }
  creds
}

#' Get/Set IO stream buffer size
#' 
#' Get/Set IO stream buffer size, the buffer size can be set at any time
#' but only takes effect on the connections created after the change. The default 
#' value is 1024 *1024 bytes (1 Mega bytes) for both read and write connections.
#' 
#' @param buff_size Integer. The buffer size
#' @examples 
#' gcs_get_input_stream_buff()
#' gcs_get_output_stream_buff()
#' @rdname buffer_size
#' @export
gcs_input_stream_buff<-function(buff_size){
  package_settings[["input_buff_len"]] <- as.double(buff_size)
}
#' @rdname buffer_size
#' @export
gcs_output_stream_buff<-function(buff_size){
  buff_size <- as.double(buff_size)
  if(buff_size < 256*1024){
    warning("The buffer size should be at least 256KB")
    buff_size <- 256*1024
  }
  package_settings[["output_buff_len"]] <- as.double(buff_size)
}
#' @rdname buffer_size
#' @export
gcs_get_input_stream_buff<-function(){
  package_settings[["input_buff_len"]]
}
#' @rdname buffer_size
#' @export
gcs_get_output_stream_buff<-function(){
  package_settings[["output_buff_len"]]
}



