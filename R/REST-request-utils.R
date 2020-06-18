get_headers <- function(...,user_pay){
    user_project <- .billing_project(user_pay = user_pay)
    add_headers(
        Authorization = get_token(),
        userProject = user_project,
        ...
    )
}

## Catch errors for the REST
catch_error <- function(r) {
    if (status_code(r) == 403 || status_code(r) == 401) {
        stop(
            "An error has occured when sending a REST request(Error code: ",
            status_code(r), "). \n",
            "A possible reason is that you do not have the access rights, ",
            "please set your credentials via 'gcs_cloud_auth()'.\n"
        )
    }
    if(status_code(r) == 400){
        stop(
            "An error has occured when sending a REST request(Error code: ",
            status_code(r), "). \n",
            "A possible reason is that the file you are trying to access ",
            "requires a billing project but your did not enable this feature ",
            "in the function call."
        )
    }
    stop_for_status(r)
    invisible(r)
}


## convert a list to an url-like format
##########################
## Example
##########################
## .list_to_url(list(b="bucket",o="object"))
## #[1] "b/bucket/o/object"
## .list_to_url(list(uploadType="resumable",name="object"), isQuery=TRUE)
## #[1] "uploadType=resumable&name=object"
##########################
## Special condition handling
##########################
## If x contains any null value, it will be ignored
## If x contains any "", the list name will be preserved.
.list_to_url <- function(x, isQuery = FALSE){
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


## construct a json url
##########################
## Parameter descriptions: 
##########################
## ...: the additional parameters that will be append to the url
## query: The query paramters
## version: the api version
## upload: whether it is an upload api
##########################
## Example
##########################
## https://storage.googleapis.com/upload/storage/v1/b/myBucket/o?uploadType=resumable
## .json_url(b="mybucket", o=NULL, query=list(uploadType="resumable"), upload=TRUE)
## [1] "https://storage.googleapis.com/upload/storage/b/mybucket/o?uploadType=resumable"
.json_url <- function(..., query = NULL, version = "v1", upload = FALSE, user_pay = FALSE){
    if(user_pay){
        if(is.null(query))
            query <- list()
        query[["userProject"]] <- .billing_project(user_pay = user_pay)
    }
    base <- "https://storage.googleapis.com"
    if(upload)
        url_type <- "/upload/storage"
    else
        url_type <- "/storage"
    ## the additional characters that will be append to the url
    request <- list(...)
    request_url <- .list_to_url(request, isQuery = FALSE)
    if(request_url!="")
        request_url <- paste0("/", request_url)
    query_url <- .list_to_url(query, isQuery = TRUE)
    if(query_url!="")
        query_url <- paste0("?", query_url)
    paste0(base,url_type,"/",version,request_url,query_url)
}