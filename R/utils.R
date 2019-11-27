package_settings <- new.env()
package_settings[["credentials"]] = ""
package_settings[["input_buff_len"]] = 1024
package_settings[["output_buff_len"]] = 1024 *1024





is_scalar_character <- function( x ) {
  is.character(x) && length(x) == 1
}
is_scalar_logical <- function( x ) {
  is.logical(x) && length(x) == 1
}

#' @export
gcs_cloud_auth <- function(creds){
  package_settings[["credentials"]] <- creds
  gcs_auth(creds)
}

#' @export
gcs_get_cloud_auth <- function(){
  package_settings[["credentials"]]
}
#' @export
gcs_input_stream_buff<-function(buff_size){
  package_settings[["input_buff_len"]] <- as.double(buff_size)
}
#' @export
gcs_output_stream_buff<-function(buff_size){
  package_settings[["output_buff_len"]] <- as.double(buff_size)
}
#' @export
gcs_get_input_stream_buff<-function(){
  package_settings[["input_buff_len"]]
}
#' @export
gcs_get_output_stream_buff<-function(){
  package_settings[["output_buff_len"]]
}