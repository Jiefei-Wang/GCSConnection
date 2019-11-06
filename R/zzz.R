#' @useDynLib googleCloudStorage, .registration = TRUE
NULL



.onLoad<-function(libname,pkgname){
  creds <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if(creds!=""){
    credentialFile <- creds
  }
}