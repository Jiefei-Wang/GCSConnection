## two cases:
## 1. on-disk folder -> cloud
## 2. cloud folder -> disk/cloud
gcs_cp_folder <-
    function(from, to, from_cloud, to_cloud, 
             recursive, billing_project)
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
            billing_project = billing_project
        )
        
        all_file_names <- results$file_names
        ## Remove place holder files
        all_file_names <- all_file_names[all_file_names!=""]
        all_file_names <- all_file_names[!is_folder_path(all_file_names)]
    }
    ## copy all files from source to destination
    if (length(all_file_names) != 0) {
        from_files <- paste0(from, all_file_names)
        to_files <- paste0(to, all_file_names)
        lapply(seq_along(all_file_names), function(i) {
            gcs_cp_file(
                from = from_files[i], to = to_files[i],
                from_cloud = from_cloud, to_cloud = to_cloud,
                billing_project = billing_project
            )
        })
    }
    
}


## we must handle three cases:
## 1. on-disk file -> cloud
## 2. cloud file -> disk
## 3. cloud file -> cloud
gcs_cp_file <-
    function(from, to, from_cloud, to_cloud, billing_project)
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
        copy_data_on_cloud(from, to, billing_project = billing_project)
    }else if (from_cloud) {
        download_data_to_disk(from, to, billing_project = billing_project)
    }else if (to_cloud) {
        upload_data_from_disk(from, to, billing_project = billing_project)
    }
}

## Convert any non-character object to character
## if the object is a FileClass or FolderClass
## Extract its billing_project setting and return the result
## if users did not provide a value.
nonchar_to_char <- function(x, billing_project, missing_billing_project){
    if(!is.null(x) && !is.character(x)){
        if(is(x, "File_or_Folder")){
            if(missing_billing_project){
                billing_project <- .class_billing_project(x)
            }
            x <- as.character(x)
        }
    }
    list(x = x, billing_project = billing_project)
}


get_billing_project <- function(x){
    if(is.logical(x)){
        billing_project <- .billing_project(requester_pays = x)
    }else{
        billing_project <- x
    }
    billing_project
}