catch_error<-function(r){
    if (status_code(r) == 403||status_code(r) == 401) {
        message(
            "An error has occured when sending a REST requestion\n",
            "One possible reason is that you do not have the access right, ", 
            "please set your credential via `gcs_cloud_auth`"
        )
    }
    stop_for_status(r)
    invisible(r)
}



JSON_URL <- function(bucket, file = NULL) {
    bucket <- URLencode(bucket)
    if(is.null(file)){
        paste0("https://storage.googleapis.com/", bucket)
    }else{
        file <- URLencode(file)
        paste0("https://storage.googleapis.com/", bucket, "/", file)
    }
}
XML_URL <- function(bucket, file = NULL) {
    bucket <- URLencode(bucket)
    if(is.null(file)){
        paste0("https://",bucket,".storage.googleapis.com/")
    }else{
        file <- URLencode(file)
        paste0("https://",bucket,".storage.googleapis.com/",file)
    }
}


JSON_upload_URL <- function(bucket, file, resumable = TRUE) {
    bucket <- URLencode(bucket)
    file <- URLencode(file)
    if(resumable)
        upload_type <- "resumable"
    else
        upload_type <- "media"
    paste0(
        "https://storage.googleapis.com/upload/storage/v1/b/",
        bucket,
        "/o?uploadType=",upload_type,"&name=",
        file
    )
}



get_range <- function(start, end) {
    paste0("bytes=", start, "-", end)
}



download_data <- function(url, start, end) {
    auth <- get_token()
    r <- GET(url,
             add_headers(Authorization = auth,
                         range = get_range(start, end)))
    
    catch_error(r)
    content(r, as = "raw")
}




start_upload <-
    function(url, content_type = "application/octet-stream") {
        r <- POST(
            url,
            add_headers(
                Authorization = get_token(),
                `X-Upload-Content-Type` = content_type
            )
        )
        
        catch_error(r)
        headers(r)$location
    }

upload_data <- function(signed_url, data, start, end, final = FALSE) {
    final <- final || is.null(data)
    if (final) {
        if (is.null(data)) {
            range <- paste0("bytes */", end + 1L)
        } else{
            range <- paste0("bytes ", start, "-", end, "/", end + 1L)
        }
    } else{
        range <- paste0("bytes ", start, "-", end, "/*")
    }
    
    r <- PUT(
        signed_url,
        add_headers(
            `Content-Length` = length(data),
            `Content-Range` = range
        ),
        body = data
    )
    if (final) {
        catch_error(r)
    } else if (status_code(r) != 308) {
        catch_error(r)
    }
    r
}


stop_upload <- function(signed_url, data_length) {
    r <- DELETE(signed_url,
                add_headers(`Content-Length` = 0))
    catch_error(r)
    r
}

get_file_size <- function(url) {
    r <- HEAD(url, add_headers(Authorization = get_token()))
    catch_error(r)
    as.double(headers(r)$`content-length`)
}



get_current_range <- function(signed_url) {
    r <- PUT(signed_url,
             add_headers(`Content-Range` = "bytes */*"))
    if (is.null(headers(r)$range)) {
        if (is.null(content(r)) || is.null(content(r)$size))
            stop("Invalid response")
        else
            as.double(content(r)$size)
    } else{
        as.double(strsplit(headers(r)$range, '-')[[1]][2])
    }
}

copy_data_on_cloud <- function(from, to){
    auth <- get_token()
    from_ul <- paste0(from$bucket,"/",from$file)
    to_url <- JSON_URL(to$bucket, to$file)
    r <- PUT(
        to_url,
        add_headers(
            Authorization = get_token(),
            `x-goog-copy-source` = from_ul
        ),
        body = NULL
    )
    catch_error(r)
}


download_data_to_disk <- function(bucket, file , disk_path){
    url <- JSON_URL(bucket,file)
    auth <- get_token()
    
    r <- GET(url,
             add_headers(Authorization = auth),
             write_disk(disk_path, overwrite =TRUE))
    if (status_code(r) == 403) {
        stop(
            "An error has occured when reading from the connection\n",
            "It seems like you are not authorized, please set your credential via `gcs_cloud_auth`"
        )
    }
    catch_error(r)
}

upload_data_from_disk <- function(disk_path, bucket, file){
    url <- JSON_upload_URL(bucket, file, resumable = FALSE)
    r <- POST(
        url,
        add_headers(
            Authorization = get_token()
        ),
        body = upload_file(disk_path)
    )
    catch_error(r)
}


#full_path_vector: either a path to a folder/file or an empty string
list_files <-function(full_path_vector, delimiter = "/"){
    bucket <- full_path_vector[1]
    path_string <- get_combined_path(full_path_vector[-1], is_folder = TRUE)
    
    url <- JSON_URL(bucket)
    url=paste0(url,"/?delimiter=",delimiter,"&prefix=",path_string)
    auth <- get_token()
    r <- GET (
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
    query_result <- XML::xmlToList(xmlParse(content(r)))
    files <- query_result[names(query_result)=="Contents"]
    file_names <- vapply(files, function(x) x$Key, character(1),USE.NAMES =FALSE)
    file_sizes <- vapply(files, function(x) x$Size, character(1),USE.NAMES =FALSE)
    folders <- query_result[names(query_result)=="CommonPrefixes"]
    folder_names <- vapply(folders,function(x) x$Prefix, character(1),USE.NAMES =FALSE)
    ## Remove the prefix
    file_names <- substring(file_names,nchar(path_string)+1)
    folder_names <- substring(folder_names,nchar(path_string)+1)
    
    list(file_names = file_names, 
         file_sizes = file_sizes,
         folder_names = folder_names)
}

get_file_meta <- function(bucket,file){
    url<-JSON_URL(bucket,file)
    auth <- get_token()
    r <- HEAD (
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
    file_header <- headers(r)
    file_header
}

delete_file<-function(bucket,file){
    url<-JSON_URL(bucket,file)
    auth <- get_token()
    r <- DELETE(
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
}
