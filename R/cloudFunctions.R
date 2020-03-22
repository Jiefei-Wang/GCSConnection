#' Connection to google cloud storage
#'
#' This function creates an R connection to a file on google cloud storage.
#' A service account credentials is required for accessing private data, the
#' credentials can be set via `gcs_cloud_auth`
#'
#' @param description character(1). The name of the file that you want to connect to.
#' It can be either the file name or a full path to the file.
#' @param open character(1). A description of how to open the connection.
#' See details for possible values. If not specified, the default value will be
#' "rb" if a credential is set or "rbp" if not.
#' @param encoding character(1). The encoding of the input/output stream of a connection.
#' Currently the parameter `encoding` should be either `native.enc` or `UTF8`. see `?connections` for more detail.
#' @param bucket character(1). The name of the bucket that the file is located in. If not supplied,
#' value in `gcs_get_global_bucket()` will be used. If a full path to the file is provided in `description`,
#' this parameter will be ignored.
#'
#' @details
#' Possible values for the argument `open` are the combination of the following characters:
#'
#' "r" or "w" : read or write mode. The GCS connection cannot be in both read and write modes.
#'
#' "t" or "b" : text or binary mode. If not specified, the default is text mode.
#'
#' @examples
#' ## Open for reading the public Landsat data
#' ## on google cloud storage in text mode
#'
#' file <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
#' con <- gcs_connection(description = file, open = "rt")
#' readLines(con, n = 4L)
#' close(con)
#' @return A connection
#' @export
gcs_connection <- function(description,
                           open = "rb",
                           encoding = getOption("encoding"),
                           bucket = NULL) {
    stopifnot(
        is_scalar_character_or_null(bucket),
        is_scalar_character(description),
        is_scalar_character(open),
        is_scalar_character(encoding)
    )
    
    ## get the file name and bucket name from description
    file_info <- digest_path(description, bucket)
    file <- file_info$file
    bucket <- file_info$bucket
    description <- get_google_URI(bucket, file)
    
    
    ## If unable to get the bucket name, use the default setting
    if (is.null(bucket))
        bucket <- googleCloudStorageR::gcs_get_global_bucket()
    
    token <- get_token()
    
    UTF8 <- identical(encoding, "UTF8")
    isText <- !grepl("b", open, fixed = TRUE)
    isRead <- grepl("r", open, fixed = TRUE)
    isWrite <- grepl("w", open, fixed = TRUE)
    
    if (isRead && isWrite) {
        stop("The connection must be in either read or write mode but not both.")
    }
    
    if (isRead) {
        bufferLength <- gcs_get_read_buff()
    } else{
        bufferLength <- gcs_get_write_buff()
    }
    
    
    autoOpen = TRUE
    
    get_bucket_connection(
        bucket = bucket,
        file = file,
        isRead = isRead,
        istext = isText,
        UTF8 = UTF8,
        autoOpen = autoOpen,
        buffLength = bufferLength,
        description = description,
        openMode = open
    )
}


#' copy files to and from buckets
#' 
#' The function supports moving files or folders from bucket to bucket, 
#' disk to bucket and bucket to disk. 
#' Note that the existing destination file will be overwritten.
#' 
#' @param from,to Character(1). The path to the folder/file. 
#' At least one path must be a google URI. The function will do its 
#' best to guess whether the path is a path to a file or a folder, but it is 
#' recommended to explicitly add a "/" at the end of the path for 
#' the folder path to exclude the possible mistake. 
#' @param recursive logical(1). Whether copy the files in the subfolders.
#' @return No return value
#' @examples 
#' tmp_path <- tempdir()
#' ## Download a file to a disk
#' gcs_cp("gs://genomics-public-data/NA12878.chr20.sample.bam", tmp_path)
#' ## Check the file existance
#' file.exists(file.path(tmp_path, "NA12878.chr20.sample.bam"))
#' 
#' ## Download all files in a path.
#' ## The files in the subfolders will not be copied due to `recursive = FALSE`
#' folder_path <- file.path(tmp_path, "example")
#' gcs_cp("gs://genomics-public-data/", folder_path, recursive = FALSE)
#' ## Check the file existance
#' list.files(folder_path)
#' 
#' @export
gcs_cp <- function(from, to, recursive = TRUE){
    from_cloud <- is_google_uri(from)
    to_cloud <- is_google_uri(to)
    if(!from_cloud&&!to_cloud)
        stop("Hey, I am a google cloud package. ",
             "Why do you use me to manage your disk file?")
    from <- standardize_file_path(from, check_type = TRUE)
    to <- standardize_file_path(to, check_type = FALSE)
    
    from_folder <- endsWith(from, "/")
    to_folder <- endsWith(to, "/")
    
    if(from_folder&&!to_folder){
        to <- paste0(to, "/")
        to_folder <- TRUE
    }
    if(!from_folder&&to_folder){
        file_name <- basename(from)
        to <- paste0(to, file_name)
        to_folder <- FALSE
    }
    gcs_cp_internal(from = from, to = to, 
                    from_cloud = from_cloud, to_cloud = to_cloud,
                    is_folder = from_folder, recursive = recursive)
}
gcs_cp_internal <- function(from, to, from_cloud,to_cloud,is_folder, recursive){
    if(is_folder){
        if(from_cloud){
            ## If the source is in the cloud
            ## Read file names in cloud and download them
            info <- decompose_google_URI(from)
            results <- list_files(info$full_path_vector)
            ## recursively copy all files in subfolders
            if(recursive){
                subfolder_names <- results$folder_names
                if(length(subfolder_names)!=0){
                    from_subfolder <- paste0(from,subfolder_names)
                    to_subfolder <- paste0(to,subfolder_names)
                    lapply(seq_along(subfolder_names),function(i)
                        gcs_cp_internal(from = from_subfolder[i], to = to_subfolder[i],
                                        from_cloud = from_cloud, to_cloud = to_cloud,
                                        is_folder = TRUE, recursive = recursive))
                }
            }
            ## get the files in the folder
            subfile_names <- results$file_names
            ind <- which(subfile_names=="")
            if(length(ind)!=0){
                warning("Non-standard file path is found:\n",
                        info$URI,"\n",
                        "this file will not be downloaded.")
                subfile_names <- subfile_names[-ind]
            }
        }else{
            ## If the source is in local disk
            ## get the files in the folder
            subfile_names <- list.files(from,recursive = TRUE)
        }
        ## copy all files from source to destination
        if(length(subfile_names)!=0){
            from_subfile <- paste0(from,subfile_names)
            to_subfile <- paste0(to,subfile_names)
            lapply(seq_along(subfile_names),function(i)
                gcs_cp_internal(from = from_subfile[i], to = to_subfile[i],
                                from_cloud = from_cloud, to_cloud = to_cloud,
                                is_folder = FALSE, recursive = FALSE))
        }
    }else{
        ## If path is a single file
        if(from_cloud){
            info <- decompose_google_URI(from)
            from <- list(
                bucket = info$bucket,
                file = info$path_string
            )
        }
        if(to_cloud){
            info <- decompose_google_URI(to)
            to <- list(
                bucket = info$bucket,
                file = info$path_string
            )
        }else{
            dir.create(dirname(to), showWarnings = FALSE,recursive=TRUE)
        }
        if(from_cloud&&to_cloud){
            copy_data_on_cloud(from, to)
            return(invisible())
        }
        if(from_cloud){
            download_data_to_disk(from$bucket, from$file, to)
            return(invisible())
        }
        if(to_cloud){
            upload_data_from_disk(from, to$bucket, to$file)
            return(invisible())
        }
    }
    return(invisible())
    
}




#' List bucket/folder/object
#' 
#' Get a list of objects in a bucket/folder if the path ends with `delimiter`.
#' Otherwise, get a description of a file.
#' 
#' 
#' @param path Character(1), the path to the bucket/folder/file,
#' seperated by `delimiter`. 
#' @param delimiter Logical(1), whether to use `/` as a path delimiter
#' @param recursive Logical(1), whether recursively query all subdirectory.
#' If TRUE, all information of the subdirectories will be downloaded. The 
#' time cost can be significantly reduced if the value is FALSE. The parameter
#' only works with bucket/folder.
#' @param depth Integer(1), the depth of the reursive search
#' @examples 
#' gcs_dir(path = "genomics-public-data/")
#' @return 
#' A `FolderClass` object or a `FileClass` object
#' @export
gcs_dir<-function(path, delimiter =  TRUE ,recursive = FALSE, depth = 2L){
    info <-  decompose_google_URI(path)
    ## Determine whether the path is a file path or a folder path
    ## If delimiter is not used, the path must be a file path
    if(!info$is_folder||!delimiter){
        ## If the file ends with a `/`
        ## Add the missing `/`
        .makeFileClass(full_path_vector = info$full_path_vector)
    }else{
        .makeFolderClass(full_path_vector = info$full_path_vector,
                         recursive = recursive,
                         depth = depth)
    }
}





#' Get/Set google credentials
#'
#' Authenticate with Google Cloud Storage. You can download the JSON credential
#' file from Google Gloud Platform. The package will search for the credentials
#' from evironment variables `GOOGLE_APPLICATION_CREDENTIALS` or `GCS_AUTH_FILE`
#' when it is onloaded. To redo the credentials initialization process
#' after the package is loaded. Simply call the `gcs_cloud_auth` function
#'  with no argument.
#'
#'
#' @param json_file character(1). A JSON file that can be used to authenticate with
#' Google Cloud Storage. If the value is `NULL`, the current credential will be erased.
#' @param gcloud logical. Whether use gcloud to authenticate with Google Cloud Storage.
#' If the value is `TRUE`, the parameter `json_file` will be ignored.
#' @param email Character(1) or NULL. For gcloud only. Account to get the access token for.
#' If not specified, the current active account in gcloud will be used.
#' @details
#' When the package is loaded, it first searches the credential file from the enviroment
#' variable `GOOGLE_APPLICATION_CREDENTIALS`. If the credentials is not found, the environment variable
#' `GCS_AUTH_FILE` will be used intead. If both variables are not specified. Users need to specify the
#' credentials by calling `gcs_cloud_auth` function.
#'
#' @rdname authentication
#' @return
#' gcs_cloud_auth : No return value
#' 
#' gcs_get_cloud_auth : An S3 `auth` class containing credentials information
#' @examples
#' ## Default authentication process
#' gcs_cloud_auth()
#' gcs_get_cloud_auth()
#' @export
gcs_cloud_auth <- function(json_file,
                           gcloud = FALSE,
                           email = NULL) {
    scope <- "https://www.googleapis.com/auth/devstorage.full_control"
    if (gcloud) {
        package_settings[["gcloud_token_time"]] <- Sys.time()
        package_settings[["gcloud_account"]] <- email
        update_gcloud_token()
        package_settings[["gcloud_credentials"]] <- TRUE
    } else{
        if (missing(json_file)) {
            json_file <- get_credentials_from_environment()
        }
        if (!is.null(json_file)) {
            tryCatch({
                package_settings[["credentials"]] <-
                    googleAuthR::gar_auth_service(json_file = json_file,
                                                  scope = scope)
            },
            warning = function(w) warning("Do the authentication with the following warning:\n",
                                          w),
            error = function(e) warning("Fail to do the authentication with the following message:\n",
                                        e)
            
            )
        } else{
            package_settings[["credentials"]] <- NULL
        }
        package_settings[["gcloud_credentials"]] <- FALSE
    }
}

#' @rdname authentication
#' @export
gcs_get_cloud_auth <- function() {
    token <- get_token()
    x <- list(
        token = token,
        `gcloud auth` = package_settings[["gcloud_credentials"]],
        `gcloud account` = package_settings[["gcloud_account"]]
    )
    structure(x, class = "auth")
}

#' @param x Used for the S3 `print` function only
#' @param ... Used for the S3 `print` function only
#' @rdname authentication
#' @export
print.auth <- function(x, ...) {
    if (is.null(x$token)) {
        cat("Token:\tNULL\n")
    } else{
        cat("Token:\n", x$token, "\n")
    }
    if (!x$`gcloud auth`) {
        cat("authen source:\tJSON file\n")
    } else{
        cat("authen source:\tgcloud\n")
        if (is.null(package_settings[["gcloud_account"]])) {
            cat("authen account:\tDefault\n")
        } else{
            cat("authen account:\t", package_settings[["gcloud_account\n"]], "\n")
        }
    }
}



#' Get/Set read/write connection buffer size
#'
#' Get/Set read/write connection buffer size, the buffer size can be set at any time
#' but only takes effect on the connections created after the change. The default
#' value is 1024 *1024 bytes (1 Mega bytes) for both read and write connections.
#'
#' @param buff_size Integer. The buffer size
#' @return
#' Get functions: the current buffer size.
#' Set functions: the previous buffer size.
#' @examples
#' gcs_get_read_buff()
#' gcs_get_write_buff()
#' @rdname buffer_size
#' @export
gcs_set_read_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- package_settings[["input_buff_len"]]
    if (buff_size < 256 * 1024) {
        warning("The buffer size is too small, it may impact the performance")
    }
    package_settings[["input_buff_len"]] <- as.integer(buff_size)
    invisible(old_size)
}
#' @rdname buffer_size
#' @export
gcs_set_write_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- package_settings[["output_buff_len"]]
    buff_size <- as.integer(buff_size)
    if (buff_size < 256 * 1024) {
        warning("The buffer size must be at least 256Kb!")
        buff_size <- 256L * 1024L
    }
    package_settings[["output_buff_len"]] <- as.integer(buff_size)
    invisible(old_size)
}
#' @rdname buffer_size
#' @export
gcs_get_read_buff <- function() {
    package_settings[["input_buff_len"]]
}
#' @rdname buffer_size
#' @export
gcs_get_write_buff <- function() {
    package_settings[["output_buff_len"]]
}

