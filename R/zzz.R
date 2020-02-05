#' @useDynLib GCSConnection, .registration = TRUE
#' @importFrom googleCloudStorageR gcs_get_global_bucket
#' @importFrom  googleAuthR gar_auth_service
#' @import httr
#' @import Rcpp
#' @importFrom XML xmlToList xmlParse 
#' @importFrom methods setClass setMethod new
NULL


.onLoad <- function(libname, pkgname) {
    gcs_cloud_auth()
    pkg_namespace <- getNamespace(pkgname)
    C_package_onLoad(pkg_namespace)
}
