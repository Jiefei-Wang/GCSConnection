

my_ceiling <- function(x, digit) {
    ceiling(x * 10^digit) / 10^digit
}


is_folder_path <- function(x){
    endsWith(x, .delimiter())
}

split_folder_path <- function(x) {
    res <- strsplit(x, .delimiter(), fixed = TRUE)[[1]]
    res
}


split_file_path <- function(x) {
    res <- strsplit(x, .delimiter(), fixed = TRUE)[[1]]
    if (endsWith(x, .delimiter())) {
        res[length(res)+1] <- ""
    }
    res
}


## The path is a vector of names without bucket
get_combined_path <- function(path_vec, is_folder) {
    combined_path <- paste0(path_vec, collapse = .delimiter())
    if (is_folder &&
        length(path_vec) != 0 &&
        !endsWith(combined_path, .delimiter())) {
        combined_path <- paste0(combined_path, .delimiter())
    }
    combined_path
}


## Convert size in byte to a character format for print
printable_size <- function(size_list) {
    result <- rep("", length(size_list))

    ind <- size_list < 10^3
    result[ind] <- paste0(size_list[ind], "B")

    ind <- size_list >= 10^3 & size_list < 10^6
    result[ind] <- paste0(my_ceiling(size_list[ind] / 10^3, digit = 1), "KB")

    ind <- size_list >= 10^6 & size_list < 10^9
    result[ind] <- paste0(my_ceiling(size_list[ind] / 10^6, digit = 1), "MB")

    ind <- size_list >= 10^9 & size_list < 10^12
    result[ind] <- paste0(my_ceiling(size_list[ind] / 10^9, digit = 1), "GB")

    ind <- size_list >= 10^12
    result[ind] <- paste0(my_ceiling(size_list[ind] / 10^12, digit = 1), "TB")

    result
}


is_google_uri <- function(x) {
    if (startsWith(x, "gcs://")) {
        stop("The URI should start with `gs://`")
    }
    startsWith(x, "gs://")
}

is_uri_folder_path <- function(x) {
    path <- split_folder_path(x)
    length(path) == 1 || endsWith(x, .delimiter())
}


standardize_google_uri <- function(x){
    if (!startsWith(x, "gs://")){
        paste0("gs://", x)
    }else{
        x
    }
}

get_google_uri <- function(bucket, file, full_path_vector = NULL) {
    if (!is.null(full_path_vector)) {
        bucket <- full_path_vector[1]
        file <- full_path_vector[-1]
    }
    file_string <- get_combined_path(file, is_folder = FALSE)
    paste0("gs://", bucket, .delimiter(), file_string)
}


decompose_google_uri <- function(x, is_folder = NULL) {
    x_std <- standardize_google_uri(x)
    
    x <- substring(x_std, first = 6)
    
    if (is.null(is_folder)) {
        is_folder <- is_uri_folder_path(x)
    }

    if (is_folder) {
        full_path_vector <- split_folder_path(x)
    } else {
        full_path_vector <- split_file_path(x)
    }

    bucket <- full_path_vector[1]
    path_vector <- full_path_vector[-1]
    path_string <- get_combined_path(path_vector, is_folder)

    list(
        uri = x,
        bucket = bucket,
        path_vector = path_vector,
        full_path_vector = full_path_vector,
        path_string = path_string,
        is_folder = is_folder
    )
}


# digest_path <- function(description, bucket = NULL) {
#     if (is_google_uri(description)) {
#         bucket <- strsplit(description, "/")[[1]][3]
#         file <- sub(paste0("gs://", bucket, "/"), "", description)
#     } else {
#         file <- description
#     }
#     list(file = file, bucket = bucket)
# }


## The input should be either a file path on a disk or a google cloud
## URL used by gcs_cp
## it will check the existance of the file/folder
## and add a trailing slash if it is a folder.
standardize_file_path <- function(x, billing_project = NULL) {
    is_cloud_path <- is_google_uri(x)
    if (!is_cloud_path) {
        x_std <- normalizePath(x, winslash = .delimiter(), mustWork = FALSE)
        ## Add "/" at the end if it is a folder
        if (!endsWith(x_std, .delimiter()) && file.exists(x_std)) {
            info <- file.info(x_std)
            if (info$isdir) {
                x_std <- paste0(x_std, .delimiter())
            }
        }
    }
    else {
        info <- decompose_google_uri(x)
        if (is.na(info$bucket)) {
            stop("Illegal path: ", x)
        }
        x_std <- info$uri
        if (!endsWith(x_std, "/") && exist_folder(info$full_path_vector,billing_project)) {
            x_std <- paste0(x_std, "/")
        }
    }
    x_std
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
    gcloud_cmd <- c("auth", "print-access-token",
                    .gcloud_account())
    tryCatch({
        .credentials(
            system2("gcloud", gcloud_cmd, stdout = TRUE)
        )
    },
    warning = function(w) {
        stop(paste0(w$message, "\n"))
    })
}


get_token <- function() {
    if (.is_gcloud()) {
        ## refresh token after 50 minutes
        if (Sys.time() - .gcloud_token_time() > 60 * 50) {
            update_gcloud_token()
        }
        paste0("Bearer ", .credentials())
    } else {
        creds <- .credentials()
        if (is.null(creds)) {
            return(NULL)
        }
        if (!creds$validate()) {
            creds$refresh()
        }
        paste0(
            creds$credentials$token_type,
            " ",
            creds$credentials$access_token
        )
    }
}


get_credentials_from_environment <- function() {
    creds <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    if (creds == "") {
        creds <- Sys.getenv("GCS_AUTH_FILE")
    }
    if (creds == "") {
        return(NULL)
    } else {
        return(creds)
    }
}

