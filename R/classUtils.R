##########################################
## Folder class accessor
##########################################
.full_path_vector<-function(x)x@full_path_vector
.file_types<-function(x) x@file_types
.file_names<-function(x) x@file_names
.file_sizes<-function(x) x@file_sizes
.recursive <- function(x) x@recursive
.depth <- function(x) x@depth
.cache<-function(x) x@cache



`.full_path_vector<-`<-function(x,value){
    x@full_path_vector <- value
    x
}
`.file_types<-` <- function(x, value){
    x@file_types <- value
    x
}
`.file_names<-` <- function(x, value){
    x@file_names <- value
    x
}
`.file_sizes<-` <- function(x, value){
    x@file_sizes <- value
    x
}
`.recursive<-`<- function(x, value){
    x@recursive <- value
    x
}
`.depth<-` <- function(x, value){
    x@depth <- value
    x
}

`.cache<-` <- function(x, value){
    x@cache <- value
    x
}


##########################################
## Folder class utils
##########################################
.full_single_char_path<-function(x){
    delimitor <- "/"
    path <- .full_path_vector(x)
    paste0(paste0(path,collapse = delimitor),delimitor)
}
.total_size <- function(x){
    sizes <- .file_sizes(x)
    file_sizes <- sum(as.numeric(sizes[sizes!="*"&.file_types(x)=="file"]))
    file_sizes
}



## Refresh the list of files in a folder
refresh_list<-function(x){
    delimiter <- "/"
    full_path_vector <- .full_path_vector(x)
    recursive<- .recursive(x)
    bucket_name <- full_path_vector[1]
    
    query_result <- list_files(full_path_vector)
    .file_types(x)<-c(
        rep("file",length(query_result$file_names)),
        rep("folder",length(query_result$folder_names))
    )
    .file_sizes(x)<-c(query_result$file_sizes,
                      rep("*",length(query_result$folder_names)))
    all_names<-c(query_result$file_names,query_result$folder_names)
    .file_names(x) <- all_names
    ## Check if there is any file end with /
    ## Somehow someone did do it. Wired
    ind <- which(.file_names(x) == "")
    if(length(ind)!=0){
        if(.file_sizes(x)[ind]!=0)
            warning("Non-standard end of the file name(a slash) has been found, it will be ignored:\n",
                    all_names[ind]
            )
        .file_names(x) <- .file_names(x)[-ind]
        .file_sizes(x) <- .file_sizes(x)[-ind]
        .file_types(x) <- .file_types(x)[-ind]
    }
    
    remove(list= ls(.cache(x)),envir = .cache(x))
    if(.recursive(x)&&x@depth>0){
        lapply(seq_along(.file_names(x)),function(i) x[[i]])
    }
    x
}

## return the name of x[i], i can be either numeric or character
## If return NULL, x[i] does not exist
match_name<-function(x,i,exact){
    if(is(x,"FolderClass")){
        all_names <- .file_names(x)
    }else{
        all_names <- names(x)
    }
    if(is.numeric(i)){
        name <- all_names[i]
    }else{
        if(!exact){
            index <- which(startsWith(all_names,i))
            if(length(index)!=0){
                distance <- abs(nchar(all_names[index])-nchar(i))
                name <- all_names[index[which.min(distance)]]
            }else{
                name<- NULL
            }
        }else{
            index <- which(all_names%in%i)
            if(length(index)!=0){
                name <- all_names[index[1]]
            }else{
                if(!endsWith(i,"/")){
                    i <- paste0(i,"/")
                    return(match_name(x,i,exact))
                }
                name<- NULL
            }
        }
    }
    name
}

##########################################
## file class utils
##########################################
.bucket_name<-function(x)x@bucket_name
.file_name<-function(x)x@file_name
.file_size<-function(x)x@file_size
.file_type<-function(x)x@file_type
.URI <-function(x)x@URI
.lastModified <-function(x)x@lastModified
`.bucket_name<-`<-function(x,value) {
    x@bucket_name<-value
    x
}
`.file_name<-`<-function(x,value){
    x@file_name <- value
    x
}
`.file_size<-`<-function(x,value){
    x@file_size <- value
    x
}
`.file_type<-`<-function(x,value){
    x@file_type <- value
    x
}
`.URI<-`<-function(x,value){
    x@URI <- value
    x
}
`.lastModified<-`<-function(x,value){
    x@lastModified <- value
    x
}