#' @useDynLib googleCloudStorageStream, .registration = TRUE
#' @import googleCloudStorageR
#' @import reticulate
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
  ## Tell R that these modules need to be loaded.
  # import("google.cloud.storage", delay_load = TRUE)
  # import("google.auth.transport.requests", delay_load = TRUE)
  # import("google.resumable_media", delay_load = TRUE)
  source_python(paste0(libname,'/',pkgname,'/python/utils.py'),envir= python_env)
}

