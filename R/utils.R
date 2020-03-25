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


is.folder_path<- function(x){
    path <- split_folde_path(x)
    endsWith(x,"/") || length(path)==1
}
split_folde_path <- function(x){
    res <- strsplit(x,"/",fixed = TRUE)[[1]]
    res
}
split_file_path<-function(x){
    res <- strsplit(x,"/",fixed = TRUE)[[1]]
    if(endsWith(x,"/")){
        res[length(res)] <- paste0(res[length(res)],"/")
    }
    res
}

## The path is a vector of names without bucket
get_combined_path <- function(path_vec, is_folder){
    combined_path <- paste0(path_vec,collapse="/")
    if(is_folder&&length(path_vec)!=0&&!endsWith(combined_path,"/"))
        combined_path <- paste0(combined_path,"/")
    combined_path
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
    if(startsWith(x,"gcs://")){
        stop("The URI should start with `gs://`")
    }
    startsWith(x,"gs://")
}
get_google_URI <- function(bucket, file, full_path_vector = NULL) {
    if(!is.null(full_path_vector)){
        bucket <- full_path_vector[1]
        file <- full_path_vector[-1]
    }
    paste0("gs://", bucket, "/", paste0(file,collapse="/"))
}
decompose_google_URI<-function(x, is_folder = NULL){
    if(is_google_uri(x)){
        x <- substring(x,first = 6)
    }else{
    }
    if(is.null(is_folder)){
        is_folder <- is.folder_path(x)
    }
    if(is_folder){
        full_path_vector <- split_folde_path(x)
    }else{
        full_path_vector <- split_file_path(x)
    }
    bucket <- full_path_vector[1]
    path_vector <- full_path_vector[-1]
    path_string <- get_combined_path(path_vector,is_folder)
    URI <- get_google_URI(full_path_vector = full_path_vector)
    
    list(URI = URI,
         bucket = bucket,
         path_vector = path_vector,
         full_path_vector = full_path_vector,
         path_string = path_string,
         is_folder = is_folder)
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
## The input should be either a file path in disk or a google cloud URI
## Used by gcs_cp
standardize_file_path<- function(x){
    is_cloud_path <- is_google_uri(x)
    if(!is_cloud_path){
        x_std <- normalizePath(x, winslash ="/" ,mustWork = FALSE)
        ## Add "/" at the end if it is a folder
        if(!endsWith(x_std, "/")&&file.exists(x_std)){
            info <- file.info(x_std)
            if(info$isdir)
                x_std =paste0(x_std,"/")
        }
    }
    else{
        info <- decompose_google_URI(x)
        if(is.na(info$bucket))
            stop("Illigal path: ",x)
        x_std <- info$URI
        if(!endsWith(x_std, "/") && exist_folder(info$full_path_vector)){
            x_std =paste0(x_std,"/")
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






