## two cases:
## 1. on-disk folder -> cloud
## 2. cloud folder -> disk/cloud
gcs_cp_folder <-
    function(from, to, from_cloud, to_cloud, recursive, user_pay)
{
    if (!from_cloud) {
        ## If the source is on a disk
        ## get the files in the folder
        all_file_names <- list.files(from, recursive = recursive)
    } else {
        ## If the source is in the cloud
        ## Read file names in cloud
        info <- decompose_google_uri(from)
        if (recursive) {
            delimiter <- ""
        } else {
            delimiter <- .delimiter()
        }
        
        results <- list_files(
            info$full_path_vector, delimiter = delimiter,
            user_pay = user_pay
        )
        
        all_file_names <- results$file_names
        ## If there are too many files,
        ## ask the use before downloading them
        if (length(all_file_names) == 1000) {
            answer <- readline(
                prompt = paste(
                    "More than 1000 files will be downloaded,",
                    "are you sure to continue?[y/n]: "
                ))
            answer <- tolower(answer)
            if (answer == "n") {
                return()
            } else {
                repeat{
                    token <- results$next_page_token
                    results <- list_files(
                        info$full_path_vector,
                        delimiter = delimiter,
                        next_page_token = token,
                        user_pay = user_pay
                    )
                    
                    all_file_names <- c(
                        all_file_names,
                        results$file_names
                    )
                    
                    if (length(results$file_names) != 1000) {
                        break
                    }
                }
            }
        }
        ## Check if the file names contain any error
        ind <- which(all_file_names == "")
        if (length(ind) != 0 && results$file_sizes[ind] != "0") {
            warning(
                "Non-standard file path is found:\n",
                info$uri, "\n",
                "this file will not be downloaded."
            )
            all_file_names <- all_file_names[-ind]
        }
    }
    ## copy all files from source to destination
    if (length(all_file_names) != 0) {
        from_files <- paste0(from, all_file_names)
        to_files <- paste0(to, all_file_names)
        lapply(seq_along(all_file_names), function(i) {
            gcs_cp_file(
                from = from_files[i], to = to_files[i],
                from_cloud = from_cloud, to_cloud = to_cloud,
                user_pay = user_pay
            )
        })
    }
    
}


## we must handle three cases:
## 1. on-disk file -> cloud
## 2. cloud file -> disk
## 3. cloud file -> cloud
gcs_cp_file <-
    function(from, to, from_cloud, to_cloud, user_pay)
{
    if (from_cloud) {
        info <- decompose_google_uri(from)
        from <- info$full_path_vector
    }
    if (to_cloud) {
        info <- decompose_google_uri(to)
        to <- info$full_path_vector
    } else {
        dir.create(
            dirname(to), showWarnings = FALSE, recursive = TRUE
        )
    }
    
    if (from_cloud && to_cloud) {
        copy_data_on_cloud(from, to, user_pay = user_pay)
    }else if (from_cloud) {
        download_data_to_disk(from, to, user_pay = user_pay)
    }else if (to_cloud) {
        upload_data_from_disk(from, to, user_pay = user_pay)
    }
}

## Convert any non-character object to character
## if the object is a FileClass or FolderClass
## Extract its user_pay setting and return the result
## if users did not provide a value.
nonchar_to_char <- function(x, user_pay, missing_user_pay){
    if(!is.null(x) && !is.character(x)){
        if(is(x, "File_or_Folder")){
            if(missing_user_pay){
                user_pay <- .class_user_pay(x)
            }
            x <- as.character(x)
        }
    }
    list(x = x, user_pay = user_pay)
}
