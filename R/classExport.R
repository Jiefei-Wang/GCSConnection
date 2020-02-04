#' @export
setClass("BucketClass",representation = list(bucket = "character"),contains = "list")
#' @export
setClass("FileClass",representation = list(details = "logical"),contains = "list")

.BucketClass<-function(bucket,max_files){
    files <- list_files(bucket,max_files)
    file_objects <- lapply(files,.FileClass)
    names(file_objects) <- as.character(lapply(file_objects,.file))
    bucketObject <- new("BucketClass",file_objects, bucket = bucket)
    bucketObject
}

.FileClass <-function(file_info){
    file_info$copy_to<-function(destination){
        gcs_cp(file_info$URI, destination)
    }
    file_info$delete <-function(quite = FALSE){
        if(!quite){
            message("Do you want to delete the file? This operation cannot be undone[y/n]:")
            answer <- readline()
            if(tolower(answer)=="n") return()
        }
        delete_file(file_info$bucket,file_info$file)
    }
    
    
    new("FileClass",file_info)
}

setMethod("show", signature("BucketClass"), function(object){
    nFile <- length(object)
    cat(nFile, " files in the bucket `",object@bucket,"`:\n",sep ="")
    
    file_names <- rep("",length(object))
    file_sizes <- rep(-1,length(object))
    for(i in seq_along(object)){
        file_names[i]=.file(object[[i]])
        file_sizes[i]=.size(object[[i]])
    }
    info = data.frame(name = file_names,
                      size = printable_size(file_sizes))
    print(info)
    invisible(object)
})


setMethod("show", signature("FileClass"), function(object){
    cat("File `",.file(object),"`:\n",sep ="")
    cat("Bucket:",.bucket(object),"\n")
    cat("Size:",printable_size(.size(object))," \n")
    cat("URI:",.URI(object),"\n")
    cat("Last modified:",.lastModified(object),"\n")
    invisible(object)
})

.bucket<-function(x){
    if(is(x,"BucketClass")){
        x@bucket
    }else{
        x$bucket
    }
}

.file <- function(x) x$file
.size <- function(x) x$size
.URI<-function(x)x$URI
.lastModified<-function(x) x$LastModified