##########################################
## Folder class accessor
##########################################
.full_path_vector <- function(x) x@full_path_vector


.file_types <- function(x) x@file_types


.file_names <- function(x) x@file_names


.file_sizes <- function(x) x@file_sizes


.depth <- function(x) x@depth


.cache <- function(x) x@cache

.is_folders <- function(x) .file_types(x)=="folder"

.is_files <- function(x) !.is_folders(x)

.class_billing_project <- function(x) x@billing_project


`.full_path_vector<-` <- function(x, value) {
    x@full_path_vector <- value
    x
}


`.file_types<-` <- function(x, value) {
    x@file_types <- value
    x
}


`.file_names<-` <- function(x, value) {
    x@file_names <- value
    x
}


`.file_sizes<-` <- function(x, value) {
    x@file_sizes <- value
    x
}




`.depth<-` <- function(x, value) {
    x@depth <- value
    x
}


`.cache<-` <- function(x, value) {
    x@cache <- value
    x
}
`.class_billing_project<-` <- function(x, value) {
    x@billing_project <- value
    x
}

##########################################
## Folder class utils
##########################################
.full_single_char_path <- function(x) {
    delimitor <- "/"
    path <- .full_path_vector(x)
    paste0(paste0(path, collapse = delimitor), delimitor)
}


.total_size <- function(x) {
    sizes <- .file_sizes(x)
    file_sizes <- sum(
        as.numeric(sizes[sizes != "*" & .file_types(x) == "file"])
    )
    file_sizes
}


## Refresh the list of files in a folder
refresh_list <- function(x) {
    delimiter <- .delimiter()
    full_path_vector <- .full_path_vector(x)
    bucket_name <- full_path_vector[1]
    
    query_result <- list_files(full_path_vector, billing_project = .class_billing_project(x))
    .file_types(x) <- c(
        rep("file", length(query_result$file_names)),
        rep("folder", length(query_result$folder_names))
    )
    .file_sizes(x) <- c(
        query_result$file_sizes,
        rep("*", length(query_result$folder_names))
    )
    all_names <- c(query_result$file_names, 
                   query_result$folder_names)
    .file_names(x) <- all_names
    ## Check if there is any file end with /
    ## Somehow someone did do it. Wired
    ind <- which(.file_names(x) == "")
    if (length(ind) != 0) {
        if (.file_sizes(x)[ind] != 0) {
            warning(
                "Non-standard end of the file name(a slash) has been found",
                "it will be ignored:\n",
                all_names[ind]
            )
        }
        .file_names(x) <- .file_names(x)[-ind]
        .file_sizes(x) <- .file_sizes(x)[-ind]
        .file_types(x) <- .file_types(x)[-ind]
    }
    
    remove(list = ls(.cache(x)), envir = .cache(x))
    if (x@depth > 0) {
        lapply(seq_along(.file_names(x)), function(i) x[[i]])
    }
    x
}


## return the index of x[i], i can be either numeric or character
## If return NULL, x[i] does not exist
match_name <- function(all_names, i, exact) {
    ## if i is an index, just return it
    if (is.numeric(i)) {
        return(i)
    }
    ## if i is a character
    if (!exact) {
        index <- which(startsWith(all_names, i))
        if (length(index) != 0) {
            distance <- abs(nchar(all_names[index]) - nchar(i))
            index <- index[which.min(distance)]
        } else {
            index <- NULL
        }
    } else {
        index <- which(all_names %in% i)
        if (length(index) != 0) {
            index <- index[1]
        }else{
            index <- NULL
        }
    }
    index
}


## This function will handle `~` `..` `.` symbols in a path
get_absolute_path <- function(base_path_vector, i){
    if(startsWith(i, .delimiter())){
        stop("Illegal path:",i)
    }
    ## if the path start with ~, go to the
    ## bucket root
    if(startsWith(i, "~")){
        i <- sub(paste0("^~",.delimiter(),"?"), "", i)
        base_path_vector <- base_path_vector[1]
    }
    ## If there is nothing left, return the bucket name
    if(i == ""){
        return(base_path_vector)
    }
    
    if(length(grep("~", i))==1){
        stop("illegal path, the symbol `~` should only be used in the start")
    }
    
    ## Add a delimiter
    if (i %in% c("..", ".")) {
        i <- paste0(i, .delimiter())
    }
    
    if (length(grep(.delimiter(), i)) != 0) {
        i_vector <- split_file_path(i)
        ## remove . symbol
        i_vector <- i_vector[i_vector != "."]
        full_path_vector <- c(base_path_vector, i_vector)
        ## process .. symbol
        full_path_vector <- process_parent_symbol(full_path_vector)
    }else{
        full_path_vector <- c(base_path_vector, i)
    }
    
    ## if the path ends with a delimiter, the last
    ## element in the vector will be an empty character
    ## we remove it and add a delimiter to the second last element
    n <- length(full_path_vector)
    if(n >= 2 && full_path_vector[n] == ""){
        full_path_vector[n-1] <- paste0(full_path_vector[n-1], .delimiter())
        full_path_vector <- full_path_vector[-n]
    }
    full_path_vector
}


# n <- length(full_path_vector)
# if (n == 1) {
#     ## If switch between bucket
#     if (!exact && .full_path_vector(x)[1] != full_path_vector) {
#         warning("argument 'exact' is ignored ",
#                 "when matching the path: ",
#                 origin_sub)
#     }
#     return(.makeFolderClass(full_path_vector))
# }
# if (n == 0) {
#     return(.makeFolderClass(full_path_vector))
# } else {
#     x_new <- .makeFolderClass(full_path_vector[-n])
#     
#     google_uri <- get_google_uri(
#         full_path_vector = full_path_vector[-n]
#     )
#     
#     .cache(x)[[google_uri]] <- x_new
#     
#     return(x_new[[full_path_vector[n], exact = exact]])
# }



## handle `..` symbol
process_parent_symbol <- function(full_path_vector) {
    removed_path <- rep(FALSE, length(full_path_vector))
    for (i in seq_along(full_path_vector)) {
        if (full_path_vector[i] == "..") {
            count <- 2
            for (j in rev(seq_len(i))) {
                if (!removed_path[j]) {
                    removed_path[j] <- TRUE
                    count <- count - 1
                }
                if (count == 0) {
                    break
                }
            }
            if (count != 0) {
                stop(
                    "cannot go the the parent directory, ",
                    "you are in the root path!"
                )
            }
        }
    }
    full_path_vector[!removed_path]
}

