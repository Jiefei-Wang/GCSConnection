package_settings <- new.env()

package_settings[["credentials"]] <- NULL

package_settings[["json_path"]] <- NULL

package_settings[["requester_pays"]] <- FALSE

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

.requester_pays <- function(x){
    if(missing(x))
        package_settings[["requester_pays"]]
    else
        package_settings[["requester_pays"]] <- as.logical(x)
}

## requester_pays: Wether to return a billing project.
##           If FALSE, NULL value will be returned
## showError: Whether to show an error when the defult
##            billing project is NULL.
.billing_project <- function(x, requester_pays = TRUE, showError = TRUE){
    if(missing(x)){
        res <- package_settings[["billing_project"]]
        if(showError && is.null(res) && requester_pays){
            stop("The billing project is not set, ",
                 "please set it via `gcs_set_billing_project()`")
        }
        if(requester_pays)
            res
        else
            NULL
    }
    else
        package_settings[["billing_project"]] <- as.character(x)
}

.gcloud_account <- function(x){
    if(missing(x)||is.null(x))
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

.json_path <- function(x){
    if(missing(x))
        package_settings[["json_path"]]
    else
        package_settings[["json_path"]] <- as.character(x)
}

