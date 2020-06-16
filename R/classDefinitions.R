#' Folder class
#'
#' View and access files. You can change the current directory by `$`
#' and `[[` operators. The symbol `..` can be used to go to the parent
#' folder of a folder object.
#'
#' @export
.FolderClass <- setClass(
    "FolderClass",
    representation = list(
        full_path_vector = "character",
        file_types = "character",
        file_names = "character",
        file_sizes = "character",
        recursive = "logical",
        depth = "numeric",
        cache = "environment"
    )
)


## full_path_vector: The first element is the bucket name,
## the rest is a vector of folder names
.makeFolderClass <-
    function(full_path_vector, recursive = FALSE, depth = 2L)
{
    if (length(full_path_vector) == 0) {
        full_path_vector <- character(0)
    }
    x <- .FolderClass()
    .full_path_vector(x) <- full_path_vector
    .recursive(x) <- recursive
    .depth(x) <- depth
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
    file_sizes <- .file_sizes(object)

    file_sizes[file_sizes != "*"] <- printable_size(
        as.numeric(file_sizes[file_sizes != "*"])
    )

    info <- data.frame(
        Name = .file_names(object),
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
#' @param name,i Character(1), the name of the element
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
        if (i == "..") {
            i <- "../"
        }
        if (startsWith(i, "/")) {
            i <- substring(i, 2)
        }
        if (startsWith(i, "./")) {
            i <- substring(i, 3)
        }
        if (length(grep("/.", i)) != 0 || startsWith(i, "../")) {
            info <- decompose_google_URI(i)
            path_vector <- info$full_path_vector
            path_vector <- path_vector[path_vector != ""]
            full_path_vector <- c(.full_path_vector(x), path_vector)
            full_path_vector <- find_true_path(full_path_vector)
            n <- length(full_path_vector)
            if (n == 1) {
                ## If switch between bucket
                if (!exact && .full_path_vector(x)[1] != full_path_vector) {
                    warning("argument 'exact' is ignored ",
                            "when matching the path: ",
                            origin_sub)
                }
                return(.makeFolderClass(full_path_vector))
            }
            if (n == 0) {
                return(.makeFolderClass(full_path_vector))
            } else {
                x_new <- .makeFolderClass(full_path_vector[-n])

                google_URI <- get_google_URI(
                    full_path_vector = full_path_vector[-n]
                )

                .cache(x)[[google_URI]] <- x_new

                return(x_new[[full_path_vector[n], exact = exact]])
            }
        }
    }

    ## If the index is just a single file/folder name
    name <- match_name(x, i, exact)
    if (is.null(name)) {
        return(NULL)
    }
    all_names <- .file_names(x)
    index <- which(all_names == name)
    ## If the cache has stored the element, return it directly
    if (!is.null(.cache(x)[[name]])) {
        return(.cache(x)[[name]])
    }
    if (.file_types(x)[index] == "folder") {
        path <- c(.full_path_vector(x), substring(name, 1, nchar(name) - 1))
        result <- .makeFolderClass(
            full_path_vector = path,
            recursive = .recursive(x),
            depth = .depth(x) - 1
        )
    } else {
        path <- c(.full_path_vector(x), name)
        result <- .makeFileClass(full_path_vector = path)
    }
    .cache(x)[[name]] <- result
    result
})


#' @inherit base::names
#' @export
setMethod("names", signature("FolderClass"), function(x) {
    all_names <- .file_names(x)
    index <- endsWith(all_names, "/")
    all_names[index] <- substring(
        all_names[index], 1,
        nchar(all_names[index]) - 1
    )
    all_names
})


#' File class
#'
#' The properties of file class object can be accessed via `$` and
#' `[[` operators.  The symbol `..` can be used to go to the parent
#' folder of a file class object.
#'
#' @export
.FileClass <- setClass(
    "FileClass",
    representation = list(
        full_path_vector = "character",
        bucket_name = "character",
        file_name = "character",
        file_size = "character",
        file_type = "character",
        URI = "character",
        lastModified = "character"
    ), contains = "list"
)


## full_path_vector is a vector of folder and file names
.makeFileClass <- function(full_path_vector) {
    x <- .FileClass()
    .full_path_vector(x) <- full_path_vector
    .bucket_name(x) <- full_path_vector[1]
    .file_name(x) <- paste0(full_path_vector[-1], collapse = "/")

    file_info <- get_file_meta(full_path_vector)
    .file_size(x) <- file_info$`size`
    .file_type(x) <- file_info$`contentType`
    .lastModified(x) <- file_info$`updated`

    .URI(x) <- get_google_URI(full_path_vector = full_path_vector)
    x
}


#' Print object of class `FileClass`
#'
#' @param object an object of class `FileClass`
#'
#' @return Invisible `Object`
#' @export
setMethod("show", signature("FileClass"), function(object) {
    cat("File:  ", .file_name(object), "\n")
    cat("Bucket:", .bucket_name(object), "\n")
    cat("Size:  ", printable_size(as.numeric(.file_size(object))), " \n")
    cat("Type:  ", .file_type(object), "\n")
    cat("URI:   ", .URI(object), "\n")
    cat("Last modified:", .lastModified(object), "\n")
    invisible(object)
})


#' Get an element from `FileClass` object
#'
#' @param x an object of class `FileClass`
#' @param name,i Character(1), the name of the element
#' @return
#' A `FolderClass` object or a `FileClass` object
#' @rdname subset-FileClass-method
#' @export
setMethod("$", signature("FileClass"), function(x, name) {
    do.call("[[", args = list(x = x, i = name, exact = FALSE))
})


#' @param exact Logical(1), Controls possible partial matching of `[[`
#'     when extracting by a character(1)
#' @rdname subset-FileClass-method
#' @export
setMethod("[[", signature("FileClass"), function(x, i, exact = TRUE) {
    if (is.character(i)) {
        if (startsWith(i, "..")) {
            info <- decompose_google_URI(i)
            path_vector <- info$full_path_vector
            path_vector <- path_vector[path_vector != ""]
            full_path_vector <- c(.full_path_vector(x), path_vector)
            full_path_vector <- find_true_path(full_path_vector)
            return(.makeFolderClass(full_path_vector))
        }
    }
    name <- match_name(x, i, exact)
    if (is.null(name)) {
        return(NULL)
    }
    if (name == "copy_to") {
        func1 <- function(destination) {
            gcs_cp(.URI(x), destination)
        }
        return(func1)
    }
    if (name == "delete") {
        func2 <- function(quiet = FALSE) {
            if (!quiet) {
                message("Are you sure you want to delete this file? [y/n]:")
                answer <- readline()
                if (tolower(answer) == "n") {
                    return()
                }
            }
            delete_file(.full_path_vector(x))
        }
        return(func2)
    }
    if (name == "get_connection") {
        func3 <- function(open = "rb", encoding = getOption("encoding")) {
            gcs_connection(
                description = .URI(x), open = open,
                encoding = encoding
            )
        }
        return(func3)
    }

    if (name == "file_name") {
        file_name <- .file_name(x)
        return(basename(file_name))
    }

    func <- get(paste0(".", name))
    func(x)
})


#' @inherit base::names
#' @export
setMethod("names", signature("FileClass"),
    function(x)
{
    c("full_path_vector", "bucket_name", "file_name",
      "file_size", "file_type", "URI", "lastModified", "copy_to",
      "delete", "get_connection")
})
