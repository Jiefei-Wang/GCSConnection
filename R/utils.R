
is_scalar_character <- function( x ) {
  is.character(x) && length(x) == 1
}
is_scalar_logical <- function( x ) {
  is.logical(x) && length(x) == 1
}

#' @export
gcs_cloud_auth <- function(creds){
  package_settings[["credentials"]] <- creds
}

#' @export
gcs_get_cloud_auth <- function(){
  package_settings[["credentials"]]
}