package_settings <- new.env()
package_settings[["credentials"]] <- NULL
package_settings[["gcloud_account"]] <- NULL
package_settings[["gcloud_credentials"]] <- FALSE
package_settings[["gcloud_token_time"]] <- NULL


package_settings[["input_buff_len"]] <- 1024L * 1024L
package_settings[["output_buff_len"]] <- 1024L * 1024L



digest_path <- function(description, bucket) {
    if (grepl("gs://", description, fixed = TRUE)) {
        bucket <- strsplit(description, "/")[[1]][3]
        file <- sub(paste0("gs://", bucket, "/"), "", description)
    } else{
        file <- description
    }
    list(file = file, bucket = bucket)
}

full_path <- function(bucket, file) {
    paste0("gs://", bucket, "/", file)
}

is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1
}
is_scalar_character_or_null <- function(x) {
    is.null(x) || is_scalar_character(x)
}
is_scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1
}


update_gcloud_token <- function() {
    tryCatch({
        package_settings[["credentials"]] <-
            system2("gcloud",
                    c("auth", "print-access-token", package_settings[["gcloud_account"]]),
                    stdout = TRUE)
    }, warning = function(w) {
        stop(paste0(w$message, "\n"))
    })
}

get_token <- function() {
    if (package_settings[["gcloud_credentials"]]) {
        ## refresh token after 50 minutes
        if (Sys.time() - package_settings[["gcloud_token_time"]] > 60 * 50) {
            update_gcloud_token()
        }
        paste0("Bearer ", package_settings[["credentials"]])
    } else{
        creds <- package_settings[["credentials"]]
        if (is.null(creds)) {
            return(NULL)
        }
        if (!creds$validate()) {
            creds$refresh()
        }
        paste0(creds$credentials$token_type,
               " ",
               creds$credentials$access_token)
    }
}

get_credentials_from_environment <- function() {
    creds <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    if (creds == "") {
        creds <- Sys.getenv("GCS_AUTH_FILE")
    }
    if (creds == "") {
        return(NULL)
    } else{
        return(creds)
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
#' @param json_file character string. A JSON file that can be used to authenticate with
#' Google Cloud Storage. If the value is `NULL`, the current credential will be erased.
#' @param gcloud logical. Whether use gcloud to authenticate with Google Cloud Storage.
#' If the value is `TRUE`, the parameter `json_file` will be ignored.
#' @param email Character string or NULL. For gcloud only. Account to get the access token for.
#' If not specified, the current active account in gcloud will be used.
#' @details
#' When the package is loaded, it first searchs the credential file from the enviroment
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
gcs_read_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- package_settings[["input_buff_len"]]
    if (buff_size < 256 * 1024) {
        warning("The buffer size is too small, it may impact the performance")
    }
    package_settings[["input_buff_len"]] <- as.integer(buff_size)
    old_size
}
#' @rdname buffer_size
#' @export
gcs_write_buff <- function(buff_size = 1024L * 1024L) {
    old_size <- package_settings[["output_buff_len"]]
    buff_size <- as.integer(buff_size)
    if (buff_size < 256 * 1024) {
        warning("The buffer size must be at least 256Kb!")
        buff_size <- 256L * 1024L
    }
    package_settings[["output_buff_len"]] <- as.integer(buff_size)
    old_size
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
