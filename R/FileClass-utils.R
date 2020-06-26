
##########################################
## file class utils
##########################################
## Defined in Folder class utils:
## .full_path_vector


.bucket_name <- function(x) x@bucket_name


.file_name <- function(x) x@file_name


.file_size <- function(x) x@file_size


.file_type <- function(x) x@file_type


.uri <- function(x) x@uri


.lastModified <- function(x) x@lastModified

.url <- function(x) get_google_url(.full_path_vector(x))

`.bucket_name<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@bucket_name <- value
    x
}


`.file_name<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@file_name <- value
    x
}


`.file_size<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@file_size <- value
    x
}


`.file_type<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@file_type <- value
    x
}


`.uri<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@uri <- value
    x
}


`.lastModified<-` <- function(x, value) {
    if(is.null(value))
        value <- character(0)
    x@lastModified <- value
    x
}

