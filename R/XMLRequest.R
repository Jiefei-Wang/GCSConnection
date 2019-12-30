
download_URL <- function(bucket, file) {
    #paste0("https://",bucket,".storage.googleapis.com/",file)
    paste0("https://storage.googleapis.com/", bucket, "/", file)
}

upload_URL <- function(bucket, file) {
    paste0(
        "https://storage.googleapis.com/upload/storage/v1/b/",
        bucket,
        "/o?uploadType=resumable&name=",
        file
    )
}


get_range <- function(start, end) {
    paste0("bytes=", start, "-", end)
}



download_data <- function(url, start, end, isPublic) {
    if (isPublic)
        auth = NULL
    else
        auth = get_token()
    
    r <- GET(url,
             add_headers(Authorization = auth,
                         range = get_range(start, end)))
    if (status_code(r) == 403) {
        stop(
            "An error has occured when reading from the connection\n",
            "It seems like you are not authorized, please set your credential via `gcs_cloud_auth`"
        )
    }
    stop_for_status(r)
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
        if (status_code(r) == 401) {
            stop(
                "An error has occured when writing to the connection\n",
                "It seems like you are not authorized, please set your credential via `gcs_cloud_auth`"
            )
        }
        stop_for_status(r)
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
        stop_for_status(r)
    } else if (status_code(r) != 308) {
        stop_for_status(r)
    }
    r
}


stop_upload <- function(signed_url, data_length) {
    r <- DELETE(signed_url,
                add_headers(`Content-Length` = 0))
    stop_for_status(r)
    r
}

get_file_size <- function(url) {
    r <- HEAD(url, add_headers(Authorization = get_token()))
    if (status_code(r) == 403) {
        stop(
            "An error has occured when reading from the connection\n",
            "It seems like you are not authorized, please set your credential via `gcs_cloud_auth`"
        )
    }
    stop_for_status(r)
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