#' @useDynLib googleCloudStorageStream, .registration = TRUE
#' @import googleCloudStorageR
NULL



.onLoad<-function(libname,pkgname){
  creds <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if(creds!=""){
    setCredential(creds)
  }
}