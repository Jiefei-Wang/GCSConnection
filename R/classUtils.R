##########################################
## Folder class utils
##########################################
.full_path<-function(x)x@full_path
.file_types<-function(x) x@file_types
.file_names<-function(x)x@file_names
.file_sizes<-function(x)x@file_sizes
.total_size <- function(x){
    sizes <- .file_sizes(x)
    file_sizes <- sum(as.numeric(sizes[sizes!="*"&.file_types(x)=="file"]))
    file_sizes
}
.delimiter<-function(x)x@delimiter
.recursive <- function(x) x@recursive
.cache<-function(x) x@cache

`.full_path<-`<-function(x,value){
    x@full_path <- value
    x
}
`.file_types<-`<-function(x,value){
    x@file_types<-value
    x
}
`.file_names<-`<-function(x,value){
    x@file_names <- value
    x
}
`.file_sizes<-`<-function(x,value){
    x@file_sizes <- value
    x
}
`.delimiter<-` <- function(x,value){
    x@delimiter<- value
    x
}
`.recursive<-`<-function(x,value){
    x@recursive<- value
    x
}
`.cache<-` <- function(x,value){
    x@cache<- value
    x
}

## Refresh the list of files in a folder
refresh_list<-function(x){
    full_path <- .full_path(x)
    recursive<- .recursive(x)
    delimiter<- .delimiter(x)
    bucket_name <- full_path[1]
    folder_names <- full_path[-1]
    if(length(folder_names)!=0){
        folder_path <- paste0(paste0(folder_names,collapse =delimiter),delimiter)
    }else{
        folder_path <- ""
    }
    query_result <- list_files(bucket_name,folder_path,delimiter)
    
    .file_types(x)<-c(
        rep("file",length(query_result$file_names)),
        rep("folder",length(query_result$folder_names))
    )
    .file_names(x)<-c(query_result$file_names,query_result$folder_names)
    .file_names(x)=substring(.file_names(x),nchar(folder_path)+1)
    .file_sizes(x)<-c(query_result$file_sizes,
                      rep("*",length(query_result$folder_names)))
    remove(list= ls(.cache(x)),envir = .cache(x))
    if(.recursive(x)&&x@deep>0){
        lapply(.file_names(x),function(i)x[[i,exact=TRUE]])
    }
    x
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