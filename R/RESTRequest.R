catch_error <- function(r) {
    if (status_code(r) == 403 || status_code(r) == 401) {
        message(
            "An error has occured when sending a REST request \n",
            "possible reason is that you do not have the access rights, ",
            "please set your credentials via 'gcs_cloud_auth()'."
        )
    }
    stop_for_status(r)
    invisible(r)
}


json_url <- function(full_path_vector, method = "") {
    bucket <- URLencode(full_path_vector[1], reserved = TRUE)
    if (length(full_path_vector) > 1) {
        file <- URLencode(
            get_combined_path(
                full_path_vector[-1],
                is_folder = FALSE
            ),
            reserved = TRUE
        )

        paste0(
            "https://storage.googleapis.com/", method,
            "storage/v1/b/", bucket, "/o/", file
        )
    } else {
        paste0(
            "https://storage.googleapis.com/", method,
            "storage/v1/b/", bucket, "/o"
        )
    }
}


json_upload_url <- function(bucket, file, resumable = TRUE) {
    URL <- json_url(full_path_vector = bucket, method = "upload/")
    file <- URLencode(file, reserved = TRUE)

    paste0(
        URL, "?uploadType=",
        ifelse(resumable, "resumable", "media"),
        "&name=", file
    )
}


xml_url <- function(bucket, file = NULL) {
    bucket <- URLencode(bucket)
    if (is.null(file)) {
        paste0("https://storage.googleapis.com/", bucket)
    } else {
        file <- URLencode(file)
        paste0("https://storage.googleapis.com/", bucket, "/", file)
    }
}


get_range <- function(start, end) {
    paste0("bytes=", start, "-", end)
}


download_data <- function(url, start, end) {
    auth <- get_token()
    r <- GET(
        url,
        add_headers(
            Authorization = auth,
            range = get_range(start, end)
        )
    )
    catch_error(r)
    content(r, as = "raw")
}


start_upload <- function(url, content_type = "application/octet-stream") {
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
        } else {
            range <- paste0("bytes ", start, "-", end, "/", end + 1L)
        }
    } else {
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
    r <- DELETE(
        signed_url,
        add_headers(`Content-Length` = 0)
    )
    catch_error(r)
    r
}

get_file_size <- function(url) {
    r <- HEAD(url, add_headers(Authorization = get_token()))
    catch_error(r)
    as.double(headers(r)$`content-length`)
}


get_current_range <- function(signed_url) {
    r <- PUT(
        signed_url,
        add_headers(`Content-Range` = "bytes */*")
    )
    if (is.null(headers(r)$range)) {
        if (is.null(content(r)) || is.null(content(r)$size)) {
            stop("Invalid response")
        } else {
            as.double(content(r)$size)
        }
    } else {
        as.double(strsplit(headers(r)$range, "-")[[1]][2])
    }
}


##################################################
## For gcs_cp and gcs_dir
##################################################

copy_data_on_cloud <- function(from_full_path_vector, to_full_path_vector) {
    from_URI <- json_url(from_full_path_vector)
    to_bucket <- URLencode(to_full_path_vector[1], reserved = TRUE)

    to_file <- URLencode(
        get_combined_path(
            to_full_path_vector[-1],
            is_folder = FALSE
        ),
        reserved = TRUE
    )
    
    URI <- paste0(from_URI, "/copyTo/b/", to_bucket, "/o/", to_file)
    auth <- get_token()
    r <- POST(
        URI,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
}


download_data_to_disk <- function(full_path_vector, disk_path) {
    root_url <- json_url(full_path_vector)
    url <- paste0(root_url, "?alt=media")
    auth <- get_token()

    r <- GET(
        url,
        add_headers(Authorization = auth),
        write_disk(disk_path, overwrite = TRUE)
    )
    if (status_code(r) == 403) {
        stop(
            "An error has occured when reading from the connection\n",
            "You are not authorized via 'gcs_cloud_auth()'"
        )
    }
    catch_error(r)
}


upload_data_from_disk <- function(disk_path, full_path_vector) {
    bucket <- full_path_vector[1]
    file <- get_combined_path(full_path_vector[-1], is_folder = FALSE)
    url <- json_upload_url(bucket, file, resumable = FALSE)
    r <- POST(
        url,
        add_headers(
            Authorization = get_token()
        ),
        body = upload_file(disk_path)
    )
    catch_error(r)
}


## full_path_vector: either a path to a folder/file or an empty string
list_files <-
    function(full_path_vector, delimiter = "/", next_page_token = NULL)
{
    bucket <- full_path_vector[1]
    path_string <- get_combined_path(full_path_vector[-1], is_folder = TRUE)
    
    toor_url <- json_url(bucket)
    url <- paste0(
        toor_url, "/?delimiter=", delimiter,
        "&prefix=", URLencode(path_string, reserved = TRUE),
        "&pageToken=", next_page_token
    )
    auth <- get_token()
    r <- GET(
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)

    query_result <- jsonlite::fromJSON(content(r, as = "text"))

    file_names <- query_result$items$name
    file_sizes <- query_result$items$size
    folder_names <- query_result$prefixes
    ## Remove the prefix
    file_names <- substring(file_names, nchar(path_string) + 1)
    folder_names <- substring(folder_names, nchar(path_string) + 1)

    list(
        file_names = file_names,
        file_sizes = file_sizes,
        folder_names = folder_names,
        next_page_token = query_result$nextPageToken
    )
}


get_file_meta <- function(full_path_vector, noError = FALSE) {
    root_url <- json_url(full_path_vector)
    url <- paste0(root_url, "?alt=json")
    auth <- get_token()
    r <- GET(
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    if (status_code(r) == 404 && noError) {
        return(NULL)
    }
    catch_error(r)
    content(r)
}


exist_file <- function(full_path_vector) {
    !is.null(get_file_meta(full_path_vector, TRUE))
}


exist_folder <- function(full_path_vector) {
    res <- list_files(full_path_vector)
    length(res$file_names) != 0 || length(res$folder_names) != 0
}


delete_file <- function(full_path_vector) {
    url <- json_url(full_path_vector)
    auth <- get_token()
    r <- DELETE(
        url,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
}
