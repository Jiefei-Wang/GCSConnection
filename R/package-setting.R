package_settings <- new.env()

package_settings[["credentials"]] <- NULL

package_settings[["user_pay"]] <- FALSE

package_settings[["billing_project"]] <- NULL

package_settings[["gcloud_account"]] <- NULL

package_settings[["is_gcloud"]] <- FALSE

package_settings[["gcloud_token_time"]] <- NULL

package_settings[["input_buff_len"]] <- 1024L * 1024L

package_settings[["output_buff_len"]] <- 1024L * 1024L

package_settings[["delimiter"]] <- "/"


.credentials <- function(x){
    if(missing(x))
        package_settings[[".credentials"]]
    else
        package_settings[[".credentials"]] <- x
}

.user_pay <- function(x){
    if(missing(x))
        package_settings[["user_pay"]]
    else
        package_settings[["user_pay"]] <- as.logical(x)
}

## user_pay: Wether to return a billing project.
##           If FALSE, NULL value will be returned
## showError: Whether to show an error when the defult
##            billing project is NULL.
.billing_project <- function(x, user_pay = TRUE, showError = TRUE){
    if(missing(x)){
        res <- package_settings[["billing_project"]]
        if(showError && is.null(res) && user_pay){
            stop("The billing project is not set, ",
                 "please set it via `gcs_set_billing_project()`")
        }
        if(user_pay)
            res
        else
            NULL
    }
    else
        package_settings[["billing_project"]] <- as.character(x)
}

.gcloud_account <- function(x){
    if(missing(x))
        package_settings[["gcloud_account"]]
    else
        package_settings[["gcloud_account"]] <- as.character(x)
}

.is_gcloud <- function(x){
    if(missing(x))
        package_settings[["is_gcloud"]]
    else
        package_settings[["is_gcloud"]] <- as.logical(x)
}

.gcloud_token_time <- function(x){
    if(missing(x))
        package_settings[["gcloud_token_time"]]
    else
        package_settings[["gcloud_token_time"]] <- x
}

.input_buff_len <- function(x){
    if(missing(x))
        package_settings[["input_buff_len"]]
    else
        package_settings[["input_buff_len"]] <- as.integer(x)
}

.output_buff_len <- function(x){
    if(missing(x))
        package_settings[["output_buff_len"]]
    else
        package_settings[["output_buff_len"]] <- as.integer(x)
}

.delimiter <- function(x){
    if(missing(x))
        package_settings[["delimiter"]]
    else
        package_settings[["delimiter"]] <- as.character(x)
}


