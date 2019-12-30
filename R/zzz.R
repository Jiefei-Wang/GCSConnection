#' @useDynLib GCSConnection, .registration = TRUE
#' @importFrom googleCloudStorageR gcs_get_global_bucket
#' @importFrom  googleAuthR gar_auth_service
#' @import httr
#' @import Rcpp
NULL


.onLoad <- function(libname, pkgname) {
    gcs_cloud_auth()
    pkg_namespace <- getNamespace("GCSConnection")
    C_package_onLoad(pkg_namespace)
}
