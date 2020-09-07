#' Connection to google cloud storage
#'
#' This function creates an R connection to a file on google cloud
#' storage.  A service account credentials is required for accessing
#' private data, the credentials can be set via `gcs_cloud_auth`. If
#' the bucket requires Requester Pays, a billing project needs to be
#' set in `gcs_set_billing_project`.
#'
#' @param description A google uri to the file that you want to connect
#'     to or a FileClass object returned by `gcs_dir`. If the value is a 
#'     path(e.g. "folder1/folder2/myfile") and the argument `bucket` is
#'     missing, it will be treated as a google uri without the common 
#'     header `gs://`. If the argument `bucket` is not missing,
#'     the value in `description` will be treated as a relative path.
#'     
#' @param open character(1). A description of how to open the
#'     connection.  See details for possible values. the default 
#'     is "rb".
#' @param encoding character(1). The encoding of the input/output
#'     stream of a connection.  Currently the parameter `encoding`
#'     should be either `native.enc` or `UTF8`. see `?connections` for
#'     more detail.
#' @param bucket character(1). The name of the bucket that the file is
#'     located in. If a google uri to the file is provided in
#'     `description`, this parameter will be ignored.
#' @param billing_project logical(1) or character(1). If logical, 
#'     whether users 
#'     should pay the cost for accessing the data. The 
#'     billing project is the project ID in `gcs_get_billing_project()`.
#'     If character, it represents the project ID that will be charged
#'     by Google.
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
    function(description,
             open = "rb",
             encoding = getOption("encoding"),
             bucket = NULL,
             billing_project = gcs_get_requester_pays())
    {
        # Convert any non-character object to character
        temp <- nonchar_to_char(description,
                                billing_project = billing_project,
                                missing_billing_project = missing(billing_project))
        description <- temp$x
        billing_project <- temp$billing_project
        
        stopifnot(
            is_scalar_character_or_null(bucket),
            is_scalar_character(description),
            is_scalar_character(open),
            is_scalar_character(encoding)
        )
        
        ## if bucket is not NULL
        ## we need to update the uri in description
        if(!is.null(bucket)){
            if(is_google_uri(description))
                stop(
                    "argument `bucket` must be NULL when a google URI is provided"
                )
            else
                description <- get_google_uri(bucket, description)
        }
        
        file_info <- decompose_google_uri(description, is_folder = FALSE)
        bucket <- file_info$bucket
        file <- file_info$path_string
        description <- file_info$uri
        
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
        billing_project <- get_billing_project(billing_project)
        if(is.character(billing_project)){
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
#' @param from,to the character path to a bucket/folder/file or 
#'     a FolderClass/FileClass object returned by `gcs_dir`. At least
#'     one path must be a google URI. It is recommended to explicitly
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
gcs_cp <- function(from, to, recursive = TRUE, billing_project = gcs_get_requester_pays()) {
    missing_billing_project <- missing(billing_project)
    ## Convert any non-character object to character
    temp <- nonchar_to_char(to, 
                            billing_project = billing_project, 
                            missing_billing_project = missing_billing_project)
    to <- temp$x
    billing_project <- temp$billing_project
    ## If both from and to are File_or_Folder objects,
    ## the setting in from has a higher priority
    temp <- nonchar_to_char(from, 
                            billing_project = billing_project, 
                            missing_billing_project = missing_billing_project)
    from <- temp$x
    billing_project <- temp$billing_project
    
    ## Convert logical value to billing project ID
    billing_project <- get_billing_project(billing_project)
    
    
    from_cloud <- is_google_uri(from)
    to_cloud <- is_google_uri(to)
    if (!from_cloud && !to_cloud) {
        stop(
            "This function is for the google cloud platform, ",
            "it cannot manage your disk file."
        )
    }
    from <- standardize_file_path(from, billing_project = billing_project)
    to <- standardize_file_path(to, billing_project = billing_project)
    
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
                      recursive = recursive, billing_project = billing_project)
    } else {
        gcs_cp_file(from = from, to = to,
                    from_cloud = from_cloud, to_cloud = to_cloud, 
                    billing_project = billing_project)
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
#' @param path The character path to a bucket/folder/file or
#'     a FolderClass/FileClass object returned by `gcs_dir`.
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
gcs_dir <- function(path, delimiter = TRUE, billing_project = gcs_get_requester_pays()) {
    ## Convert any non-character object to character
    temp <- nonchar_to_char(path, 
                            billing_project = billing_project, 
                            missing_billing_project = missing(billing_project))
    path <- temp$x
    billing_project <- temp$billing_project
    
    ## Convert logical value to billing project ID
    billing_project <- get_billing_project(billing_project)
    ## If path is empty
    if(is.null(path)||
       length(path) == 0){
        path = ""
    }
    info <- decompose_google_uri(path)
    ## If delimiter is not used, the path must be a file path
    ## Otherwise, we send a request to the cloud to
    ## Determine whether the path is a file path or a folder path
    if(delimiter && exist_folder(info$full_path_vector, billing_project)){
        is_folder <- TRUE
    }else{
        is_folder <- FALSE
    }
    
    info <- decompose_google_uri(path, is_folder = is_folder)
    
    
    if (!is_folder) {
        .makeFileClass(full_path_vector = info$full_path_vector, 
                       billing_project = billing_project)
    } else {
        .makeFolderClass(
            full_path_vector = info$full_path_vector,
            billing_project = billing_project
        )
    }
}

#' Delete a file or a directory
#' 
#' Delete a file or a directory. The path to a directory 
#' *must* have a tailing slash so that the function can 
#' distinguish it from a file path.
#' 
#' @param quiet Whether to require the user's confirmation 
#'     before deleting a directory.
#' @inheritParams gcs_dir
#' @return No return value.
#' @examples 
#' ## remove the entire content in a bucket
#' ## gcs_rm("myBucket")
#' 
#' ## remove a folder in a bucket
#' ## gcs_rm("myBucket/myFolder/")
#' 
#' ## remove a file in a bucket
#' ## gcs_rm("myBucket/myFolder/myFile")
#' 
#' @export
gcs_rm <- function(path, billing_project = gcs_get_requester_pays(),
                   quiet = FALSE){
    ## Convert any non-character object to character
    temp <- nonchar_to_char(path, 
                            billing_project = billing_project, 
                            missing_billing_project = missing(billing_project))
    path <- temp$x
    billing_project <- temp$billing_project
    
    ## Convert logical value to billing project ID
    billing_project <- get_billing_project(billing_project)
    
    info <- decompose_google_uri(path)
    
    if(info$is_folder&&
        exist_folder(info$full_path_vector, billing_project)){
        results <- list_files(
            info$full_path_vector,
            delimiter = NULL,
            billing_project = billing_project
        )
        if(endsWith(info$uri,"/")){
            all_files_uri <- paste0(info$uri, results$file_names)
        }else{
            all_files_uri <- paste0(info$uri, "/", results$file_names)
        }
        if(length(all_files_uri)==0)
            return(invisible())
        if(!quiet){
            answer <- readline(
                prompt = paste(
                    length(all_files_uri),
                    " files will be deleted,",
                    "are you sure to continue?[y/n]: "
                ))
            answer <- tolower(answer)
            if (answer == "n") {
                return(invisible())
            }
        }
        for(i in all_files_uri){
            cur_file_info <- decompose_google_uri(i)
            if(cur_file_info$is_folder){
                cur_file_info$full_path_vector<-
                    c(cur_file_info$full_path_vector,
                      "")
            }
            delete_file(cur_file_info$full_path_vector, billing_project)
        }
        return(invisible())
    }
    if(exist_file(info$full_path_vector, billing_project)){
        delete_file(info$full_path_vector, billing_project)
    }
    return(invisible())
}


#' Google credentials
#'
#' Authenticate with Google Cloud Storage. You can download the JSON
#' credential file from Google Gloud Platform. The package will search
#' for the credentials from evironment variables
#' `GOOGLE_APPLICATION_CREDENTIALS` or `GCS_AUTH_FILE` when it is
#' loaded in R. If both variables are not set, the package will try
#' to get your credentials from `gcloud` program. If it fails to find
#' `gcloud`, you will use anonymous credentials. 
#' To redo the credentials initialization process after the
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
#' @param email character(1) or NULL. For gcloud only. Account to get
#'     the access token for.  If not specified, the current active
#'     account in gcloud will be used.
#' @param billing_project character(1) or NULL. The project's ID which 
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
#' @seealso requester_pays
#' @return gcs_cloud_auth : No return value
#'
#' gcs_get_cloud_auth : An S3 `auth` class containing credentials
#' information
#'
#' @examples
#' ## Default authentication process
#' gcs_cloud_auth()
#' ## Show the credentials
#' gcs_get_cloud_auth()
#' 
#' ## Anonymous credential
#' gcs_cloud_auth(NULL)
#' gcs_get_cloud_auth()
#'
#' ## Use gcloud to do the authentication
#' if(GCSConnection:::exists_gcloud()){
#'     gcs_cloud_auth(gcloud = TRUE)
#'     gcs_get_cloud_auth()
#' }
#' @export
gcs_cloud_auth <-
    function(json_file, gcloud = FALSE, email = NULL, billing_project = NULL)
    {
        quiet <- FALSE
        ## If no argument is provided, we use the
        ## default authentication process, it do the  
        ## following items in order
        ## 1. Find the credentials from the environment variable
        ## 2. gcloud
        ## 3. anonymous credential
        if (missing(json_file) && !gcloud) {
            json_file <- get_credentials_from_environment()
            ## If fail to find the JSON file and gcloud exist,
            ## use gcloud instead
            if(is.null(json_file)){
                if(exists_gcloud()){
                    gcloud <- TRUE
                    quiet <- TRUE
                }
                ## If no gcloud can be found, 
                ## gcloud = FALSE
                ## json_file = NULL
            }
        }
        if (gcloud) {
            .is_gcloud(TRUE)
            .gcloud_token_time(Sys.time())
            .gcloud_account(email)
            gcs_set_billing_project(billing_project = billing_project, gcloud = TRUE)
            success <- update_gcloud_token(quiet)
            ## If fail to get credentials from gcloud
            ## We use anonymous credential
            if(!success){
                gcs_cloud_auth(NULL)
            }
        } else {
            scope <- "https://www.googleapis.com/auth/devstorage.full_control"
            .is_gcloud(FALSE)
            ## if the json file exist
            if (!is.null(json_file)) {
                json_file <- normalizePath(json_file, mustWork = FALSE)
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
                    .json_path(json_file)
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
                ## if json_file is NULL, clear out credentials
                .json_path(NULL)
                .credentials(NULL)
                .billing_project(NULL, requester_pays = FALSE, showError = FALSE)
            }
        }
    }


#' @rdname authentication
#' @export
gcs_get_cloud_auth <- function() {
    token <- get_token()
    x <- list(
        token_exist = !is.null(token),
        gcloud_auth = .is_gcloud(),
        gcloud_account = .gcloud_account(),
        billing_project = gcs_get_billing_project()
    )
    structure(x, class = "auth")
}


#' @param x Used for the S3 `print` function only
#' @param ... Used for the S3 `print` function only
#' @rdname authentication
#' @export
print.auth <- function(x, ...) {
    ## print token
    if (x$token_exist) {
        token <- "******"
    } else {
        token <- "NULL"
    }
    if(length(x$billing_project)){
        project <- x$billing_project
    }else{
        project <- "NULL"
    }
    ## Authen source
    if (x$gcloud_auth) {
        source  <- "gcloud"
        if (is.null(x$gcloud_account)) {
            account <- "Default"
        } else {
            account <- x$gcloud_account
        }
    } else {
        source  <- "JSON file"
        account  <- character(0)
    }
    
    cat("Token:              ", token, "\n")
    cat("Billing project ID: ", project, "\n")
    cat("Auth source:        ", source, "\n")
    if (length(account)) {
        cat("Auth account:       ", account, "\n")
    }
    
}

#' Requester Pays
#' 
#' These functions allow you to set billing project, change the default 
#' billing target and check if a bucket has Requester Pays enabled. See
#' the details section in `?authentication` for more information.
#' 
#' @param x logical(1), whether the user should pay for the cost by default.
#' @param bucket character(1), the bucket name or uri
#' @param billing_project The project's ID which the bill will be sent to.
#' @param gcloud logical(1), whether to use the default billing project
#'  in gcloud. If `gcloud = TRUE`, `billing_project` must be NULL.
#' @rdname requester_pays
#' @examples 
#' gcs_get_billing_project()
#' gcs_get_requester_pays()
#' @return 
#' gcs_get_billing_project: character(1) or NULL
#' gcs_get_requester_pays: logical(1)
#' @export
gcs_set_billing_project <- function(billing_project = NULL, gcloud = FALSE){
    if(is.logical(billing_project)){
        stop("The argument <billing_project> must be a character.\n",
             "If you want to enable your billing project in every function call by default,\n",
             "you need to call `gcs_set_requester_pays(TRUE)`")
    }
    if(gcloud && !is.null(billing_project)){
        stop("billing_project is not NULL and gcloud is TRUE!")
    }
    if(gcloud){
        .billing_project(
            system2("gcloud", c("config", "get-value", "project"), stdout = TRUE)
        )
    }else{
        .billing_project(billing_project)
    }
}


#' @rdname requester_pays
#' @export
gcs_get_billing_project <- function(){
    .billing_project(showError = FALSE)
}


#' @rdname requester_pays
#' @export
gcs_get_requester_pays <- function(){
    .requester_pays()
}


#' @rdname requester_pays
#' @export
gcs_set_requester_pays <- function(x){
    .requester_pays(x)
}


#' @rdname requester_pays
#' @export
gcs_is_requester_pays <- function(bucket){
    info <- decompose_google_uri(bucket)
    bucket <- info$bucket
    .is_requester_pays(bucket)
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


