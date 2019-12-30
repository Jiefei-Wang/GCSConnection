package_settings <- new.env()
package_settings[["credentials"]] <- NULL
package_settings[["gcloud_account"]] <- NULL
package_settings[["gcloud_credentials"]] <- FALSE
package_settings[["gcloud_token_time"]] <- NULL


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
  if(package_settings[["gcloud_credentials"]]){
    ## refresh token after 50 minutes
    if(Sys.time()-package_settings[["gcloud_token_time"]]>60*50){
      tryCatch({
        package_settings[["credentials"]] <- 
          system2("gcloud", 
                  c("auth", "print-access-token",package_settings[["gcloud_account"]]), 
                  stdout = TRUE)
      },warning = function(W){
        stop(paste0(creds,"\n"))
      })
    }
    paste0("Bearer ",package_settings[["credentials"]])
  }else{
    creds <- package_settings[["credentials"]]
    if(is.null(creds)){
      return(NULL)
    }
    if(!creds$validate()){
      creds$refresh()
    }
    paste0(creds$credentials$token_type," ",creds$credentials$access_token)
  }
}


#' Get/Set google credentials
#' 
#' Authenticate with Google Cloud Storage. You can download the JSON credential 
#' file from Google Gloud Platform. The credentials can be set globally by the 
#' evironment variable `GOOGLE_APPLICATION_CREDENTIALS` or `GCS_AUTH_FILE`.
#' 
#' @param json_file character string. A JSON file that can be used to authenticate with
#' Google Cloud Storage. If the value is `NULL`, the current credential will be erased.
#' @param gcloud logical. Whether use gcloud to authenticate with Google Cloud Storage.
#' If the value is `TRUE`, the parameter `json_file` will be ignored.
#' @param email Character string or NULL. For gcloud only. Account to get the access token for. 
#' If not specified, the current active account in gcloud will be used.
#' @details 
#' When the package is loaded, it first searchs the credential file from the enviroment 
#' variable `GOOGLE_APPLICATION_CREDENTIALS`. If the credentials is not found, the environment variable
#' `GCS_AUTH_FILE` will be used intead. If both variables are not specified. Users need to specify the
#' credentials by calling `gcs_cloud_auth` function.
#' 
#' @rdname authentication
#' @return 
#' gcs_cloud_auth : No return value
#' gcs_get_cloud_auth : A list containing credentials information
#' @examples 
#' ## Authenticate with a service account
#' \dontrun{
#' gcs_cloud_auth(json_file = "/example.json")
#' }
#' @export
gcs_cloud_auth <- function(json_file = NULL, gcloud = FALSE, email = NULL){
  scope <- "https://www.googleapis.com/auth/devstorage.full_control"
  if(gcloud){
    package_settings[["gcloud_token_time"]] <- Sys.time()
    package_settings[["credentials"]] <- system2("gcloud", c("auth", "print-access-token"), stdout = TRUE)
    package_settings[["gcloud_account"]] <- email
    package_settings[["gcloud_credentials"]] <- TRUE
  }else{
    if(!is.null(json_file)){
      package_settings[["credentials"]] <- googleAuthR::gar_auth_service(json_file = json_file,
                                                                         scope = scope)
    }else{
      package_settings[["credentials"]] <- NULL
    }
    package_settings[["gcloud_credentials"]] <- FALSE
  }
}

#' @rdname authentication
#' @export
gcs_get_cloud_auth<-function(){
  token <- get_token()
  list(token = token,
             `gcloud auth` = package_settings[["gcloud_credentials"]],
             `gcloud account` = package_settings[["gcloud_account"]]
             )
}

#' Get/Set read/write connection buffer size
#' 
#' Get/Set read/write connection buffer size, the buffer size can be set at any time
#' but only takes effect on the connections created after the change. The default 
#' value is 1024 *1024 bytes (1 Mega bytes) for both read and write connections.
#' 
#' @param buff_size Integer. The buffer size
#' @return 
#' Get functions: the current buffer size.
#' Set functions: the previous buffer size.
#' @examples 
#' gcs_get_read_buff()
#' gcs_get_write_buff()
#' @rdname buffer_size
#' @export
gcs_read_buff<-function(buff_size = 1024L*1024L){
  old_size <- package_settings[["input_buff_len"]]
  if(buff_size < 256*1024){
    warning("The buffer size is too small, it may impact the performance")
  }
  package_settings[["input_buff_len"]] <- as.integer(buff_size)
  old_size
}
#' @rdname buffer_size
#' @export
gcs_write_buff<-function(buff_size = 1024L*1024L){
  old_size <- package_settings[["output_buff_len"]]
  buff_size <- as.integer(buff_size)
  if(buff_size < 256*1024){
    warning("The buffer size must be at least 256Kb!")
    buff_size <- 256L * 1024L
  }
  package_settings[["output_buff_len"]] <- as.integer(buff_size)
  old_size
}
#' @rdname buffer_size
#' @export
gcs_get_read_buff<-function(){
  package_settings[["input_buff_len"]]
}
#' @rdname buffer_size
#' @export
gcs_get_write_buff<-function(){
  package_settings[["output_buff_len"]]
}



