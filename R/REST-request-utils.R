get_headers <- function(...,billing_project){
    add_headers(
        Authorization = get_token(),
        userProject = billing_project,
        ...
    )
}

## Catch errors for the REST
catch_error <- function(r) {
    if (status_code(r) == 403 || status_code(r) == 401) {
        stop(
            "An error has occured when sending a REST request(Error code: ",
            status_code(r), "). \n",
            "Either you do not have the access rights, ",
            "or you have sent an invalid billing project\n"
        )
    }
    if(status_code(r) == 400){
        stop(
            "An error has occured when sending a REST request(Error code: ",
            status_code(r), "). \n",
            "A possible reason is that the bucket you are trying to access ",
            "requires a billing project but your did not enable this feature ",
            "in the function call."
        )
    }
    stop_for_status(r)
    invisible(r)
}


## convert a list to an uri-like format
##########################
## Example
##########################
## .list_to_uri(list(b="bucket",o="object"))
## #[1] "b/bucket/o/object"
## .list_to_uri(list(uploadType="resumable",name="object"), isQuery=TRUE)
## #[1] "uploadType=resumable&name=object"
##########################
## Special condition handling
##########################
## If x contains any null value, it will be ignored
## If x contains any "", the list name will be preserved.
.list_to_uri <- function(x, isQuery = FALSE){
    if(isQuery){
        assignment <- "="
        separater <- "&"
    }else{
        assignment <- "/"
        separater <- "/"
    }
    x <- x[!vapply(x, function(x)is.null(x)||length(x)==0, logical(1))]
    result <- lapply(
        seq_along(x),
        function(i,x){
            if(x[[i]]==""){
                names(x)[i]
            }else{
                paste0(names(x)[i], assignment, 
                       URLencode(as.character(x[[i]]), reserved = TRUE))
            }
        }, x = x)
    paste0(result, collapse = separater)
}


## construct a json uri
##########################
## Parameter descriptions: 
##########################
## ...: the additional parameters that will be append to the uri
## query: The query paramters
## version: the api version
## upload: whether it is an upload api
##########################
## Example
##########################
## https://storage.googleapis.com/upload/storage/v1/b/myBucket/o?uploadType=resumable
## .json_uri(b="mybucket", o=NULL, query=list(uploadType="resumable"), upload=TRUE)
## [1] "https://storage.googleapis.com/upload/storage/b/mybucket/o?uploadType=resumable"
.json_uri <- function(..., query = NULL, version = "v1", upload = FALSE, 
                      billing_project = NULL){
    if(!is.null(billing_project)){
        if(is.null(query))
            query <- list()
        query[["userProject"]] <- billing_project
    }
    base <- "https://storage.googleapis.com"
    if(upload)
        uri_type <- "/upload/storage"
    else
        uri_type <- "/storage"
    ## the additional characters that will be append to the uri
    request <- list(...)
    request_uri <- .list_to_uri(request, isQuery = FALSE)
    if(request_uri!="")
        request_uri <- paste0("/", request_uri)
    query_uri <- .list_to_uri(query, isQuery = TRUE)
    if(query_uri!="")
        query_uri <- paste0("?", query_uri)
    paste0(base,uri_type,"/",version,request_uri,query_uri)
}