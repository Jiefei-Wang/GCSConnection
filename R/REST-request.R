json_uri <- function(bucket, file = NULL, query = NULL,  upload = FALSE, 
                     billing_project = NULL) {
    if (!is.null(file)&&length(file)!=0) {
        file <- get_combined_path(
            file,
            is_folder = FALSE
        )
    }
    .json_uri(b = bucket, o=file, query = query, 
              upload = upload, billing_project = billing_project)
}


json_upload_uri <- function(bucket, file, resumable = TRUE, billing_project = NULL) {
    json_uri(bucket = bucket, file = "",
             query = 
                 list(uploadType = ifelse(resumable, "resumable", "media"),
                      name = get_combined_path(
                          file,
                          is_folder = FALSE
                      ),
                      userProject = billing_project
                 ), 
             upload = TRUE)
}

##################################################
## For connections
##################################################
## This xml function is for the read connection only
## The JSON API does not support reading a range
## of bytes from a file
xml_uri <- function(bucket, file) {
    bucket <- URLencode(bucket, reserved = TRUE)
    file <- URLencode(file, reserved = TRUE)
    paste0("https://storage.googleapis.com/", bucket, "/", file)
}


get_range <- function(start, end) {
    paste0("bytes=", start, "-", end)
}

download_data <- function(uri, start, end, billing_project) {
    auth <- get_token()
    r <- GET(
        uri,
        add_headers(
            Authorization = auth,
            range = get_range(start, end),
            `x-goog-user-project` = billing_project
        )
    )
    catch_error(r)
    content(r, as = "raw")
}

## For the write connection, the uri is a
## JASON API
start_upload <- function(uri, content_type = "application/octet-stream") {
    r <- POST(
        uri,
        add_headers(
            Authorization = get_token(),
            `X-Upload-Content-Type` = content_type
        )
    )
    catch_error(r)
    headers(r)$location
}


upload_data <- function(signed_uri, data, start, end, final = FALSE) {
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
        signed_uri,
        add_headers(
            `Content-Length` = length(data),
            `Content-Range` = range
        ),
        body = data
    )
    if (final ||
        (!final && status_code(r) != 308)) {
        catch_error(r)
    }
    r
}


stop_upload <- function(signed_uri, data_length) {
    r <- DELETE(
        signed_uri,
        add_headers(`Content-Length` = 0)
    )
    catch_error(r)
    r
}

get_file_size <- function(uri, billing_project) {
    r <- HEAD(uri, 
              add_headers(Authorization = get_token(),
                          `x-goog-user-project` = billing_project))
    catch_error(r)
    as.double(headers(r)$`content-length`)
}



##################################################
## For gcs_cp and gcs_dir
##################################################

copy_data_on_cloud <- function(from_full_path_vector, to_full_path_vector, 
                               billing_project = NULL) {
    from_bucket <- from_full_path_vector[1]
    from_file <- get_combined_path(
        from_full_path_vector[-1],
        is_folder = FALSE
    )
    to_bucket <- to_full_path_vector[1]
    to_file <- get_combined_path(
        to_full_path_vector[-1],
        is_folder = FALSE
    )
    
    uri <- .json_uri(b=from_bucket,o=from_file,copyTo="",
                     b=to_bucket,o=to_file,billing_project = billing_project)
    r <- POST(
        uri,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
}

download_data_to_disk <- function(full_path_vector, disk_path, billing_project = NULL) {
    bucket <- full_path_vector[1]
    file <- full_path_vector[-1]
    uri <- json_uri(bucket = bucket, file = file, 
                    query = list(alt = "media"), billing_project = billing_project)
    r <- GET(
        uri,
        add_headers(
            Authorization = get_token()
        ),
        write_disk(disk_path, overwrite = TRUE)
    )
    catch_error(r)
}


upload_data_from_disk <- function(disk_path, full_path_vector, billing_project = NULL) {
    bucket <- full_path_vector[1]
    file <- full_path_vector[-1]
    uri <- json_upload_uri(bucket, file, resumable = FALSE, 
                           billing_project = billing_project)
    r <- POST(
        uri,
        add_headers(
            Authorization = get_token()
        ),
        body = upload_file(disk_path)
    )
    catch_error(r)
}

## full_path_vector = c("bioconductor_test")
## full_path_vector = c("bioconductor")
## list_files(full_path_vector)
## full_path_vector: either a path to a folder/file or an empty string
list_files <-
    function(full_path_vector, delimiter = .delimiter(), 
             next_page_token = NULL, billing_project = NULL)
    {
        bucket <- full_path_vector[1]
        path_string <- get_combined_path(full_path_vector[-1], is_folder = TRUE)
        if(path_string != ""){
            path_string_encoded <- URLencode(path_string, reserved = TRUE)
        }else{
            path_string_encoded <- NULL
        }
        
        
        uri <- json_uri(bucket = bucket, file = "", query = list(
            delimiter = delimiter,
            prefix = path_string_encoded,
            pageToken = next_page_token
        ),
        billing_project = billing_project
        )
        r <- GET(
            uri,
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
        ## Remove the postfix
        if(!is.null(delimiter)){
            folder_names <- substring(folder_names, 1, 
                                      nchar(folder_names) - nchar(delimiter))
        }
        
        list(
            file_names = file_names,
            file_sizes = file_sizes,
            folder_names = folder_names,
            next_page_token = query_result$nextPageToken
        )
    }


get_file_meta <- function(full_path_vector, noError = FALSE, billing_project = NULL) {
    bucket <- full_path_vector[1]
    file <- full_path_vector[-1]
    if(length(file)==0)
        file = ""
    uri <- json_uri(bucket = bucket, file = file, 
                    query = list(alt = "json"), billing_project = billing_project)
    r <- GET(
        uri,
        add_headers(
            Authorization = get_token()
        )
    )
    if (noError && status_code(r) == 404) {
        return(NULL)
    }
    catch_error(r)
    content(r)
}


exist_file <- function(full_path_vector, billing_project = NULL) {
    !is.null(get_file_meta(full_path_vector, noError = TRUE,
                           billing_project = billing_project))
}


exist_folder <- function(full_path_vector, billing_project = NULL) {
    if(length(full_path_vector)<=1)
        return(TRUE)
    res <- list_files(full_path_vector, billing_project = billing_project)
    length(res$file_names) != 0 || length(res$folder_names) != 0
}


delete_file <- function(full_path_vector, billing_project = NULL) {
    bucket <- full_path_vector[1]
    file <- full_path_vector[-1]
    uri <- json_uri(bucket = bucket, file = file, billing_project = billing_project)
    auth <- get_token()
    r <- DELETE(
        uri,
        add_headers(
            Authorization = get_token()
        )
    )
    catch_error(r)
}

.is_requester_pays <- function(bucket){
    uri <- json_uri(bucket = bucket, query = list(fields = "billing"))
    r <- GET(
        uri,
        add_headers(
            Authorization = get_token()
        )
    )
    ## The status code must be either 400 or 200
    ## Otherwise, some error happened.
    if(status_code(r) == 400)
    if(identical(content(r)$error$message,
                  "Bucket is requester pays bucket but no user project provided.")){
        return(TRUE)
    }
    if(status_code(r) == 200){
        return(FALSE)
    }
    catch_error(r)
}
