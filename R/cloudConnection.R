package_settings <- new.env()
package_settings[["credentials"]] = NULL

#' @export
gcs_connection <-function(description, open, 
                          encoding = "UTF8",
           credentials = gcs_get_cloud_auth(),
           bucket = gcs_get_global_bucket()){
  stopifnot(
    is_scalar_character(credentials),
    is_scalar_character(bucket),
    is_scalar_character(description),
    is_scalar_character(open),
    is_scalar_character(encoding)
  )
  
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
                         buffLength = 512)
}
    






