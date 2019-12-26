#' @useDynLib googleCloudStorageStream, .registration = TRUE
#' @import googleCloudStorageR
#' @importFrom  googleAuthR gar_auth_service
#' @import httr
NULL



.onLoad<-function(libname,pkgname){
  creds <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if(creds!=""){
    gcs_cloud_auth(creds)
  }else{
    creds <- Sys.getenv("GCS_AUTH_FILE")
    if(creds!="")
      gcs_cloud_auth(creds)
  }
  pkg_namespace <- getNamespace("googleCloudStorageStream")
  C_package_onLoad(pkg_namespace)
}

