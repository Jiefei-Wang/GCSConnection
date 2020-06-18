#' Connection to google cloud storage
#'
#' This function creates an R connection to a file on google cloud
#' storage.  A service account credentials is required for accessing
#' private data, the credentials can be set via `gcs_cloud_auth`. If
#' the bucket requires Requester Pays, a billing project needs to be
#' set in `gcs_set_billing_project`.
#'
#' @param description character(1). The Google Cloud URL to the file 
#'     that you want to connect to. If the value is a file path
#'     (e.g. "folder1/folder2/myfile"), the bucket name will be provided
#'     separately in the paramter `bucket`.
#' @param open character(1). A description of how to open the
#'     connection.  See details for possible values. the default 
#'     is "rb".
#' @param encoding character(1). The encoding of the input/output
#'     stream of a connection.  Currently the parameter `encoding`
#'     should be either `native.enc` or `UTF8`. see `?connections` for
#'     more detail.
#' @param bucket character(1). The name of the bucket that the file is
#'     located in. If not supplied, value in `gcs_get_global_bucket()`
#'     will be used. If a full path to the file is provided in
#'     `description`, this parameter will be ignored.
#' @param user_pay logical(1). Whether users should be responsible
#'     for the cost associated with accessing the data. If the value
#'     is `TRUE`, the charge will be sent to the billing project. See
#'     `?gcs_get_billing_project()` for more details.
#'
#' @details Possible values for the argument `open` are the
#'     combination of the following characters:
#'
#' "r" or "w" : read or write mode. The GCS connection cannot be in
#'  both read and write modes.
#'
#' "t" or "b" : text or binary mode. If not specified, the default is
#'  text mode.
#'
#' @examples
#'
#' ## Open for reading the public Landsat data # on google cloud
#' ## storage in text mode
#'
#' f <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
#' con <- gcs_connection(description = f, open = "rt")
#' readLines(con, n = 4L)
#' close(con)
#'
#' @return A connection
#'
#' @export
gcs_connection <-
    function(description, open = "rb",
             encoding = getOption("encoding"), bucket = NULL,
             user_pay = gcs_get_user_pay())
    {
        ## Convert any non-character object to character
        temp <- nonchar_to_char(description, 
                                       user_pay = user_pay, 
                                       missing_user_pay = missing(user_pay))
        description <- temp$x
        user_pay <- temp$user_pay
        
        stopifnot(
            is_scalar_character_or_null(bucket),
            is_scalar_character(description),
            is_scalar_character(open),
            is_scalar_character(encoding)
        )
        
        ## get the file name and bucket name from description
        if (is_google_uri(description) && !is.null(bucket)) {
            stop("The argument `bucket` must be NULL when a google URI is provided")
        }
        
        if (is_google_uri(description)) {
            file_info <- decompose_google_uri(description, is_folder = FALSE)
            bucket <- file_info$bucket
            file <- file_info$path_string
        } else {
            ## If unable to get the bucket name, use the default setting
            if (is.null(bucket)) {
                bucket <- googleCloudStorageR::gcs_get_global_bucket()
            }
            file <- description
        }
        
        description <- get_google_uri(bucket, file)
        
        token <- get_token()
        
        UTF8 <- identical(encoding, "UTF8")
        is_text <- !grepl("b", open, fixed = TRUE)
        is_read <- grepl("r", open, fixed = TRUE)
        is_write <- grepl("w", open, fixed = TRUE)
        
        if (is_read && is_write) {
            stop("connection must be in either read or write mode, but not both.")
        }
        
        if (is_read) {
            buff_length <- gcs_get_read_buff()
        } else {
            buff_length <- gcs_get_write_buff()
        }
        
        ## add the billing project
        billing_project <- .billing_project(user_pay = user_pay)
        if(user_pay){
            description <- paste0(description,"(Billing project enabled)")
        }
        
        auto_open <- TRUE
        
        get_bucket_connection(
            bucket = bucket,
            file = file,
            is_read = is_read,
            is_text = is_text,
            UTF8 = UTF8,
            auto_open = auto_open,
            buff_length = buff_length,
            description = description,
            open_mode = open,
            billing_project = billing_project
        )
    }


#' copy files to and from buckets
#'
#' The function supports moving files or folders from bucket to
#' bucket, disk to bucket and bucket to disk.  Note that the existing
#' destination file will be overwritten.
#'
#' @param from,to Character(1). The path to the folder/file.  At least
#'     one path must be a google URL. It is recommended to explicitly
#'     add a trailing "/" if the parameter is a path to a folder.
#' @param recursive logical(1). Whether to recursively copy the files in
#'     the subfolders.
#' @inheritParams gcs_connection
#' @return No return value
#'
#' @examples
#' tmp_path <- tempdir()
#'
#' ## Download a file to a disk
#' gcs_cp("gs://genomics-public-data/NA12878.chr20.sample.bam", tmp_path)
#'
#' ## Check the file existance
#' file.exists(file.path(tmp_path, "NA12878.chr20.sample.bam"))
#'
#' ## Download all files in a path.
#' ## The files in the subfolders will not be copied due to `recursive = FALSE`
#' folder_path <- file.path(tmp_path, "example")
#' gcs_cp("gs://genomics-public-data/", folder_path, recursive = FALSE)
#'
#' ## Check the file existance
#' list.files(folder_path)
#'
#' @export
gcs_cp <- function(from, to, recursive = TRUE, user_pay = gcs_get_user_pay()) {
    ## Convert any non-character object to character
    temp <- nonchar_to_char(to, 
                            user_pay = user_pay, 
                            missing_user_pay = missing(user_pay))
    to <- temp$x
    user_pay <- temp$user_pay
    ## If both from and to are File_or_Folder objects,
    ## the setting in from has a higher priority
    temp <- nonchar_to_char(from, 
                            user_pay = user_pay, 
                            missing_user_pay = missing(user_pay))
    from <- temp$x
    user_pay <- temp$user_pay
    
    
    from_cloud <- is_google_uri(from)
    to_cloud <- is_google_uri(to)
    if (!from_cloud && !to_cloud) {
        stop(
            "This function is for the google cloud platform, ",
            "it cannot manage your disk file."
        )
    }
    from <- standardize_file_path(from, user_pay = user_pay)
    to <- standardize_file_path(to, user_pay = user_pay)
    
    ## The path has been standardized
    from_folder <- is_folder_path(from)
    to_folder <- is_folder_path(to)
    
    if (from_folder && !to_folder) {
        to <- paste0(to, "/")
        to_folder <- TRUE
    }
    if (!from_folder && to_folder) {
        file_name <- basename(from)
        to <- paste0(to, file_name)
        to_folder <- FALSE
    }
    if (from_folder) {
        gcs_cp_folder(from = from, to = to,
                      from_cloud = from_cloud, to_cloud = to_cloud,
                      recursive = recursive, user_pay = user_pay)
    } else {
        gcs_cp_file(from = from, to = to,
                    from_cloud = from_cloud, to_cloud = to_cloud, 
                    user_pay = user_pay)
    }
    return(invisible())
}


#' List bucket/folder/object
#'
#' list objects in a bucket/folder or get the description of a file.
#' You can change the current direction via `[[` or `$` operator. `..`
#' can be used to go to the parent folder. For reducing the number of
#' request sent to the network, it is recommended to add a trailing
#' slash if the path is a folder.
#'
#' @param path Character(1), the path to the bucket/folder/file.
#' @param delimiter Logical(1), whether to use `/` as a path
#'     delimiter. If not, the path will be treated as the path to a
#'     file even when it ends with `/`
#' @inheritParams gcs_connection
#'
#' @examples
#'
#' ## List files in a bucket
#' ## Equivalent: gcs_dir(path = "gs://genomics-public-data/")
#' gcs_dir(path = "genomics-public-data/")
#'
#' ## List files in a folder
#' gcs_dir(path = "genomics-public-data/clinvar/")
#'
#' ## List the information of a file
#' gcs_dir(path = "genomics-public-data/clinvar/README.txt")
#'
#' @return
#' `FolderClass` object or a `FileClass` object
#'
#' @export
gcs_dir <- function(path, delimiter = TRUE, user_pay = gcs_get_user_pay()) {
    ## Convert any non-character object to character
    temp <- nonchar_to_char(path, 
                            user_pay = user_pay, 
                            missing_user_pay = missing(user_pay))
    path <- temp$x
    user_pay <- temp$user_pay
    ## If path is empty
    if(is.null(path)||
       length(path) == 0){
        path = ""
    }
    info <- decompose_google_uri(path)
    ## If delimiter is not used, the path must be a file path
    ## Otherwise, we send a request to the cloud to
    ## Determine whether the path is a file path or a folder path
    if(delimiter && exist_folder(info$full_path_vector, user_pay)){
        is_folder <- TRUE
    }else{
        is_folder <- FALSE
    }
    
    info <- decompose_google_uri(path, is_folder = is_folder)
    
    
    if (!is_folder) {
        .makeFileClass(full_path_vector = info$full_path_vector, user_pay = user_pay)
    } else {
        .makeFolderClass(
            full_path_vector = info$full_path_vector,
            user_pay = user_pay
        )
    }
}


#' Google credentials
#'
#' Authenticate with Google Cloud Storage. You can download the JSON
#' credential file from Google Gloud Platform. The package will search
#' for the credentials from evironment variables
#' `GOOGLE_APPLICATION_CREDENTIALS` or `GCS_AUTH_FILE` when it is
#' onloaded. To redo the credentials initialization process after the
#' package is loaded. Simply call the `gcs_cloud_auth` function with
#' no argument.
#'
#'
#' @param json_file character(1). A JSON file that can be used to
#'     authenticate with Google Cloud Storage. If the value is `NULL`,
#'     the current credential will be erased.
#' @param gcloud logical. Whether use gcloud to authenticate with
#'     Google Cloud Storage.  If the value is `TRUE`, the parameter
#'     `json_file` will be ignored. See details.
#' @param email Character(1) or NULL. For gcloud only. Account to get
#'     the access token for.  If not specified, the current active
#'     account in gcloud will be used.
#' @param billing_project Character(1) or NULL. The project's ID which 
#'     the charges will be sent to. If the value is not NULL, it will 
#'     overwrite the default setting. See details.
#' @details 
#'     **Obtaining credentials**
#'     
#'     When the package is loaded, it first searches the
#'     credential file from the enviroment variable
#'     `GOOGLE_APPLICATION_CREDENTIALS`. If the credentials is not
#'     found, the environment variable `GCS_AUTH_FILE` will be used
#'     intead. If both variables are not specified. Users need to
#'     specify the credentials by calling `gcs_cloud_auth` function.
#'     
#'     The function also works with Google Cloud SDK. If you have initialized
#'     the SDK and set up your google account, the credentials can be obtained
#'     via `gcs_cloud_auth(gcloud = TRUE)`.
#'     
#'     **Billing project**
#'     
#'     Some buckets have enabled Requester Pays, which means users are responsible
#'     for the charges associated with the data access. In this case, you must have
#'     a billing project for receiving the bills. By default, if you are using a 
#'     service account to authenticate with Google Cloud, the billing project will 
#'     be set to the project associated with the service account. 
#'     If you are using Google Cloud SDK, the billing probject will be the default 
#'     project in `gcloud`. You can also manually set the billing project via 
#'     `gcs_set_billing_project`.
#'
#' @rdname authentication
#' @return gcs_cloud_auth : No return value
#'
#' gcs_get_cloud_auth : An S3 `auth` class containing credentials
#' information
#'
#' @examples
#' ## Default authentication process
#' gcs_cloud_auth()
#' 
#' ## Show the credentials
#' gcs_get_cloud_auth()
#'
#' @export
gcs_cloud_auth <-
    function(json_file, gcloud = FALSE, email = NULL, billing_project = NULL)
    {
        scope <- "https://www.googleapis.com/auth/devstorage.full_control"
        if (gcloud) {
            .is_gcloud(TRUE)
            .gcloud_token_time(Sys.time())
            .gcloud_account(email)
            update_gcloud_token()
            gcs_set_billing_project(billing_project = billing_project, gcloud = TRUE)
        } else {
            .is_gcloud(FALSE)
            if (missing(json_file)) {
                json_file <- get_credentials_from_environment()
            }
            ## if the json file exist
            if (!is.null(json_file)) {
                tryCatch({
                    gar <- gar_auth_service(
                        json_file = json_file,
                        scope = scope
                    )                
                    .credentials(gar)
                    if(is.null(billing_project)){
                        gcs_set_billing_project(billing_project = gar$secrets$project_id)
                    }else{
                        gcs_set_billing_project(billing_project = billing_project)
                    }
                    
                },
                warning = function(w) {
                    warning(
                        "Authenticated with the following warning:\n",
                        w
                    )
                },
                error = function(e) {
                    warning(
                        "Failed to authenticate with the following message:\n",
                        e
                    )
                }
                )
            } else {
                ## if the json file does not exist, clear out credentials
                .credentials(NULL)
                .billing_project(NULL, user_pay = FALSE, showError = FALSE)
            }
        }
    }


#' @rdname authentication
#' @export
gcs_get_cloud_auth <- function() {
    token <- get_token()
    x <- list(
        token = token,
        `gcloud auth` = .is_gcloud(),
        `gcloud account` = .gcloud_account(),
        `billing project` = gcs_get_billing_project()
    )
    structure(x, class = "auth")
}


#' @param x Used for the S3 `print` function only
#' @param ... Used for the S3 `print` function only
#' @rdname authentication
#' @export
print.auth <- function(x, ...) {
    ## print token
    if (is.null(x$token)) {
        cat("Token:\tNULL\n")
    } else {
        cat("Token:\n", x$token, "\n")
    }
    ## print billing project ID
    cat("Billing project ID: ", x[["billing project"]], "\n")
    ## print authen source
    if (!x[["gcloud auth"]]) {
        cat("authen source:\tJSON file\n")
    } else {
        cat("authen source:\tgcloud\n")
        if (is.null(x[["gcloud account"]])) {
            cat("authen account:\tDefault\n")
        } else {
            cat(
                "authen account:\t",
                x[["gcloud account"]], "\n"
            )
        }
    }
}

#' @rdname authentication
#' @export
gcs_set_billing_project <- function(billing_project = NULL, gcloud = FALSE){
    if(gcloud && is.null(billing_project)){
        .billing_project(
            system2("gcloud", c("config", "get-value", "project"), stdout = TRUE)
        )
    }else{
        .billing_project(billing_project)
    }
}

#' @rdname authentication
#' @export
gcs_get_billing_project <- function(){
    .billing_project(showError = FALSE)
}
#' @rdname authentication
#' @export
gcs_get_user_pay <- function(){
    .user_pay()
}
#' @rdname authentication
#' @export
gcs_set_user_pay <- function(x){
    .user_pay(x)
}

#' Get/Set read/write connection buffer size
#'
#' Get/Set read/write connection buffer size, the buffer size can be
#' set at any time but only takes effect on the connections created
#' after the change. The default value is 1024 *1024 bytes (1 Mega
#' bytes) for both read and write connections.
#'
#' @param buff_size Integer. The buffer size
#'
#' @return
#' Get functions: the current buffer size.
#' Set functions: the previous buffer size.
#'
#' @examples
#' gcs_get_read_buff()
#' gcs_get_write_buff()
#'
#' @rdname buffer_size
#'
#' @export
gcs_set_read_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- .input_buff_len()
    if (buff_size < 256 * 1024) {
        warning("The buffer size is too small, it may impact the performance")
    }
    .input_buff_len(buff_size)
    invisible(old_size)
}


#' @rdname buffer_size
#' @export
gcs_set_write_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- .output_buff_len()
    buff_size <- as.integer(buff_size)
    if (buff_size < 256 * 1024) {
        warning("The buffer size must be at least 256Kb!")
        buff_size <- 256L * 1024L
    }
    .output_buff_len(buff_size)
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
