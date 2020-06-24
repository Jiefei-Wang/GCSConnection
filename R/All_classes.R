setClassUnion("Char_or_NULL", c("character","NULL"))

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
        depth = "numeric",
        cache = "environment",
        billing_project = "Char_or_NULL"
    )
)


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
        uri = "character",
        lastModified = "character",
        billing_project = "Char_or_NULL"
    )
)

setClassUnion("File_or_Folder", c("FolderClass","FileClass"))
