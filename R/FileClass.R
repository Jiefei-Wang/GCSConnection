## full_path_vector is a vector of folder and file names
.makeFileClass <- function(full_path_vector, user_pay = FALSE) {
    x <- .FileClass()
    .full_path_vector(x) <- full_path_vector
    .class_user_pay(x) <- user_pay
    
    .bucket_name(x) <- full_path_vector[1]
    .file_name(x) <- paste0(full_path_vector[-1], collapse = .delimiter())
    file_info <- get_file_meta(full_path_vector, noError = FALSE, user_pay = user_pay)
    .file_size(x) <- file_info$`size`
    .file_type(x) <- file_info$`contentType`
    .lastModified(x) <- file_info$`updated`
    
    .uri(x) <- get_google_uri(full_path_vector = full_path_vector)
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
    cat("URI:   ", .uri(object), "\n")
    cat("Last modified:", .lastModified(object), "\n")
    cat("user pay:", .class_user_pay(object), "\n")
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
    if (is.character(i) && !i%in% names(x)) {
        if (startsWith(i, "..")||
            startsWith(i, "~")||
            grepl(.delimiter(),i,fixed = TRUE)) {
            full_path_vector <- get_absolute_path(.full_path_vector(x), i)
            path <- paste0(full_path_vector,collapse = .delimiter())
            return(gcs_dir(path, delimiter = TRUE, user_pay = .class_user_pay(x)))
        }
    }
    name <- names(x)[match_name(names(x), i, exact)]
    if (is.null(name)) {
        return(NULL)
    }
    if (name == "copy_to") {
        func1 <- function(destination, user_pay = .class_user_pay(x)) {
            gcs_cp(.uri(x), destination, user_pay = user_pay)
        }
        return(func1)
    }
    if (name == "delete") {
        func2 <- function(quiet = FALSE, user_pay = .class_user_pay(x)) {
            if (!quiet) {
                message("Are you sure you want to delete this file? [y/n]:")
                answer <- readline()
                if (tolower(answer) == "n") {
                    return()
                }
            }
            delete_file(.full_path_vector(x), user_pay = user_pay)
        }
        return(func2)
    }
    if (name == "get_connection") {
        func3 <- function(open = "rb", encoding = getOption("encoding"),
                          user_pay = .class_user_pay(x)) {
            gcs_connection(
                description = .uri(x), open = open,
                encoding = encoding, user_pay = user_pay
            )
        }
        return(func3)
    }
    
    if (name == "file_name") {
        full_path_vector <- .full_path_vector(x)
        return(full_path_vector[length(full_path_vector)])
    }
    if (name == "user_pay") {
        return(.class_user_pay(x))
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
                "file_size", "file_type", "URI", "lastModified","user_pay",
                "copy_to", "delete", "get_connection")
          })



#' @export
setAs("FileClass", "character", 
      function(from)
      {
          get_google_uri(full_path_vector = .full_path_vector(from))
      }
)

#' Convert a FileClass object to a google URI
#' 
#' @param x FileClass object
#' @param ... not used
#' @return google URI
#' @export
setMethod("as.character", signature("FileClass"),
          function(x, ...){
              as(x,"character")
          }
)

