#' @useDynLib GCSConnection, .registration = TRUE
#' @importFrom googleCloudStorageR gcs_get_global_bucket
#' @importFrom googleAuthR gar_auth_service
#' @importFrom stats setNames
#' @import httr
#' @import Rcpp
#' @import methods
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
NULL

.onLoad <- function(libname, pkgname) {
    gcs_cloud_auth()
    pkg_namespace <- getNamespace(pkgname)
    C_package_onLoad(pkg_namespace)
}
