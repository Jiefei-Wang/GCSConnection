package_settings <- new.env()
package_settings[["python_code_path"]] <- NULL
package_settings[["initialized"]] <- FALSE
package_settings[["credentials"]] <- NULL
package_settings[["input_buff_len"]] <- 1024L*1024L
package_settings[["output_buff_len"]] <- 1024L *1024L





digest_path <- function(description, bucket){
  if(grepl("gs://",description,fixed=T)){
    bucket <- strsplit(description,"/")[[1]][3]
    file <- sub(paste0("gs://",bucket,"/"),"",description)
  }else{
    file <- description
  }
  list(file = file,bucket = bucket)
}

full_path <-function(bucket, file){
  paste0("gs://",bucket,"/",file)
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


get_token<-function(){
  req <- package_settings[["credentials"]]
  if(is.null(req)){
    return(NULL)
  }
  if(!req$validate()){
    req$refresh()
  }
  paste0(req$credentials$token_type," ",req$credentials$access_token)
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
gcs_cloud_auth <- function(json_file){
  scope <- "https://www.googleapis.com/auth/devstorage.full_control"
  
  package_settings[["credentials"]] <- googleAuthR::gar_auth_service(json_file = json_file,
                                                        scope = scope)
  # if(!is.null(creds)){
  #   gcs_auth(creds)
  # }
}



#' Get/Set IO stream buffer size
#' 
#' Get/Set IO stream buffer size, the buffer size can be set at any time
#' but only takes effect on the connections created after the change. The default 
#' value is 1024 *1024 bytes (1 Mega bytes) for both read and write connections.
#' 
#' @param buff_size Integer. The buffer size
#' @return 
#' Get functions: the current buffer size.
#' Set functions: the previous buffer size.
#' @examples 
#' gcs_get_input_stream_buff()
#' gcs_get_output_stream_buff()
#' @rdname buffer_size
#' @export
gcs_input_stream_buff<-function(buff_size){
  old_size <- package_settings[["input_buff_len"]]
  package_settings[["input_buff_len"]] <- as.integer(buff_size)
  old_size
}
#' @rdname buffer_size
#' @export
gcs_output_stream_buff<-function(buff_size){
  old_size <- package_settings[["output_buff_len"]]
  buff_size <- as.integer(buff_size)
  if(buff_size < 256*1024){
    warning("The buffer size should be at least 256KB")
    buff_size <- 256*1024
  }
  package_settings[["output_buff_len"]] <- as.integer(buff_size)
  old_size
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



