package_settings <- new.env()
package_settings[["credentials"]] <- NULL
package_settings[["gcloud_account"]] <- NULL
package_settings[["gcloud_credentials"]] <- FALSE
package_settings[["gcloud_token_time"]] <- NULL


package_settings[["input_buff_len"]] <- 1024L * 1024L
package_settings[["output_buff_len"]] <- 1024L * 1024L

myCeiling<-function(x,digit){
    ceiling(x*10^digit)/10^digit
}

## Convert size in byte to a character format for print
printable_size <-function(size_list){
    result <- rep("",length(size_list))
    ind <- size_list<10^3
    result[ind] <- paste0(size_list[ind],"B")
    ind <- size_list>=10^3&size_list<10^6
    result[ind] <- paste0(myCeiling(size_list[ind]/10^3,digit=1),"KB")
    ind <- size_list>=10^6&size_list<10^9
    result[ind] <- paste0(myCeiling(size_list[ind]/10^6,digit=1),"MB")
    ind <- size_list>=10^9&size_list<10^12
    result[ind] <- paste0(myCeiling(size_list[ind]/10^9,digit=1),"GB")
    ind <- size_list>=10^12
    result[ind] <- paste0(myCeiling(size_list[ind]/10^12,digit=1),"TB")
    result
}

is_google_uri <- function(x){
    nchar(x)>=5 && substr(x,1,5)=="gs://"
}

digest_path <- function(description, bucket = NULL) {
    if (is_google_uri(description)) {
        bucket <- strsplit(description, "/")[[1]][3]
        file <- sub(paste0("gs://", bucket, "/"), "", description)
    } else{
        file <- description
    }
    list(file = file, bucket = bucket)
}

get_google_URI <- function(bucket, file) {
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






