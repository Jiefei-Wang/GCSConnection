## full_path_vector: The first element is the bucket name,
## the rest is a vector of folder names
.makeFolderClass <-
    function(full_path_vector, depth = 1L, billing_project = NULL)
    {
        if (length(full_path_vector) == 0) {
            full_path_vector <- character(0)
        }
        x <- .FolderClass()
        .full_path_vector(x) <- full_path_vector
        .depth(x) <- depth
        .class_billing_project(x) <- billing_project
        if (length(.full_path_vector(x)) != 0) {
            x <- refresh_list(x)
        }
        x
    }


#' Print object of class `FolderClass`
#'
#' @param object an object of class `FolderClass`
#' @return invisible NULL
#' @export
setMethod("show", signature("FolderClass"), function(object) {
    n_file <- length(.file_names(object))
    if (length(.full_path_vector(object)) == 0) {
        cat("Cloud Root\n")
        return(invisible(NULL))
    }
    if (length(.full_path_vector(object)) == 1) {
        cat(n_file, " items in the bucket `",
            .full_path_vector(object), "`:\n",
            sep = ""
        )
    } else {
        cat(n_file, " items in the folder `",
            .full_single_char_path(object),
            "`:\n",
            sep = ""
        )
    }
    cat("--------------------\n")
    is_files <- .is_files(object)
    file_sizes <- .file_sizes(object)
    
    file_sizes[is_files] <- printable_size(
        as.numeric(file_sizes[is_files])
    )
    ## add a trailing slash to the folders
    file_names <- .file_names(object)
    file_names[!is_files] <- paste0(file_names[!is_files], .delimiter()) 
    
    info <- data.frame(
        Name = file_names,
        Size = file_sizes,
        stringsAsFactors = FALSE
    )
    print(info)
    cat("--------------------\n")
    size_info <- .total_size(object)
    if (length(size_info) != 0) {
        cat("Total Size : ", printable_size(size_info[1]), "\n")
    }
    invisible(NULL)
})


#' Get an element from `FolderClass` object
#'
#' @param x an object of class `FolderClass`
#' @param name,i character(1), the name of the element
#' @rdname subset-FolderClass-method
#' @return
#' A `FolderClass` object or a `FileClass` object
#' @export
setMethod("$", signature("FolderClass"), function(x, name) {
    do.call("[[", args = list(x = x, i = name, exact = FALSE))
})


#' @param exact Logical(1), Controls possible partial matching
#' of `[[` when extracting by a character(1)
#' @rdname subset-FolderClass-method
#' @export
setMethod("[[", signature("FolderClass"), function(x, i, exact = TRUE) {
    origin_sub <- i
    ## If the index is a path to a file
    if (is.character(i)) {
        full_path_vector <- get_absolute_path(.full_path_vector(x), i)
        old_path_vector <- .full_path_vector(x)
        
        n <- length(full_path_vector)
        len_diff <- n - length(old_path_vector)
        is_subpath <- grepl(paste0("^",paste(old_path_vector,collapse=.delimiter())),
                            paste(full_path_vector,collapse=.delimiter()))
        
        if(len_diff == 0 && is_subpath){
            return(x)
        }else if(len_diff != 1 || !is_subpath){
            return(
              gcs_dir(paste(full_path_vector, collapse = .delimiter()),
                      delimiter = TRUE, 
                      billing_project = .class_billing_project(x))
              )
        }else{
            ## if it is just a single file/folder names
            i <- full_path_vector[length(full_path_vector)]
        }
    }
    
    ## If the index is just a single file/folder name
    ## add an trailing delimiter if the name is a folder
    all_names <- .file_names(x)
    all_names[.is_folders(x)] <- paste0(all_names[.is_folders(x)], .delimiter())
    index <- match_name(all_names, i, exact)
    if (is.null(index)) {
        return(NULL)
    }
    name_with_slash <- all_names[index]
    name_without_slash <- .file_names(x)[index]
    ## If the cache has stored the element, return it directly
    if (!is.null(.cache(x)[[name_with_slash]])) {
        return(.cache(x)[[name_with_slash]])
    }
    if (.file_types(x)[index] == "folder") {
        path <- c(.full_path_vector(x), name_without_slash)
        result <- .makeFolderClass(
            full_path_vector = path,
            depth = .depth(x) - 1,
            billing_project = .class_billing_project(x)
        )
    } else {
        path <- c(.full_path_vector(x), name_without_slash)
        result <- .makeFileClass(full_path_vector = path, 
                                 billing_project = .class_billing_project(x))
    }
    .cache(x)[[name_with_slash]] <- result
    result
})


#' @inherit base::names
#' @export
setMethod("names", signature("FolderClass"), function(x) {
    .file_names(x)
})



#' @export
setAs("FolderClass", "character", 
      function(from)
      {
          uri <- get_google_uri(full_path_vector = .full_path_vector(from))
          if(!endsWith(uri,.delimiter())){
            uri <- paste0(uri,.delimiter())
          }
          uri
      }
)

#' Convert A FolderClass object to a dummy google URI
#' 
#' Convert A FolderClass object to a dummy google URI, this URI
#' does not exist on google, it can only be used in `gcs_cp`.
#' 
#' @param x FolderClass object
#' @param ... not used
#' @return google URI
#' @export
setMethod("as.character", signature("FolderClass"),
          function(x, ...){
    as(x,"character")
}
)