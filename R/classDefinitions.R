#' Folder class
#' 
#' view and access files. The Folder class object can be accessed
#'  via `$` and `[[` operators.
#' 
#' @export
.FolderClass <- setClass("FolderClass",representation = list(full_path = "character",
                                                             file_types = "character",
                                                             file_names = "character",
                                                             file_sizes= "character",
                                                             delimiter = "character",
                                                             recursive = "logical",
                                                             deep = "numeric",
                                                             cache = "environment"
))
## full_path: The first element is the bucket name, 
## the rest is a vector of folder names
.makeFolderClass<-function(full_path,delimiter = "/",recursive = FALSE,deep=2L){
  x <- .FolderClass()
  .full_path(x)<-full_path
  .delimiter(x) <- delimiter
  .recursive(x) <- recursive
  x@deep <- deep
  x <- refresh_list(x)
  
  x
}
#' Print object of class `FolderClass`
#' 
#' @param object an object of class `FolderClass`
#' @return invisible `object`
#' @export
setMethod("show", signature("FolderClass"), function(object){
  n_file <- length(.file_names(object))
  if(length(.full_path(object))==1){
    cat(n_file, " items in the bucket `",
        .full_path(object),"`:\n",sep ="")
  }else{
    cat(n_file, " items in the folder `",
        paste0(.full_path(object),collapse = .delimiter(object)),
        "`:\n",sep ="")
  }
  cat("--------------------\n")
  file_sizes <- .file_sizes(object)
  file_sizes[file_sizes!="*"] <- printable_size(as.numeric(file_sizes[file_sizes!="*"]))
  info = data.frame(Name = .file_names(object),
                    Size = file_sizes,stringsAsFactors = FALSE)
  print(info)
  cat("--------------------\n")
  size_info <- .total_size(object)
  if(length(size_info)!=0){
    cat("Total Size : ", printable_size(size_info[1]),"\n")
  }
  invisible(object)
})


#' Get an element from `FolderClass` object
#' 
#' @param x an object of class `FolderClass`
#' @param name,i Character(1), the name of the element
#' @rdname subset-FolderClass-method
#' @return  
#' A `FolderClass` object or a `FileClass` object
#' @export
setMethod("$",signature("FolderClass"),function(x,name){
  #print(name)
  if(!name%in%names(x)){
    stop("Cannot find the specific element `",name,"`")
  }
  if(name=="#refresh_list#"){
    return(function()refresh_list(x))
  }
  if(name == "#file_names#"){
    return(.file_names(x))
  }
  if(name=="#file_sizes#"){
    return(.file_sizes(x))
  }
  
  if(!is.null(.cache(x)[[name]])){
    return(.cache(x)[[name]])
  }
  index <- which(.file_names(x)==name)
  if(.file_types(x)[index]=="folder"){
    path <- c(.full_path(x),sub(.delimiter(x),"",name))
    result <- .makeFolderClass(path,.delimiter(x),.recursive(x),x@deep-1)
  }else{
    path <- c(.full_path(x),name)
    result <- .makeFileClass(path,.delimiter(x))
  }
  .cache(x)[[name]]<-result
  result
})
#' @param exact Logical(1), Controls possible partial matching
#' of `[[` when extracting by a character(1)
#' @rdname subset-FolderClass-method
#' @export
setMethod("[[",signature("FolderClass"),function(x,i,exact =TRUE){
  if(!exact){
    index <- pmatch(i,names(x))
    if(!is.na(index)){
      i=names(x)[index]
    }else{
      return(NULL)
    }
  }
  do.call("$",args=list(x=x,name=i))
})


#' @inherit base::names
#' @export
setMethod("names",signature("FolderClass"),function(x){
  c(.file_names(x),"#file_names#","#file_sizes#","#refresh_list#")
})


#' File class
#' 
#' The file class object can be accessed via `$` and `[[` operators.
#' @export
.FileClass = setClass("FileClass",
                      representation = list(
                        full_path = "character",
                        bucket_name = "character",
                        file_name = "character",
                        file_size = "character",
                        file_type = "character",
                        URI = "character",
                        lastModified = "character"
                      ),contains = "list")


.makeFileClass <-function(full_path,delimiter){
  x<-.FileClass()
  .full_path(x) <- full_path
  .bucket_name(x)<-full_path[1]
  .file_name(x)<-paste0(full_path[-1],collapse = delimiter)
  file_info <- get_file_meta(.bucket_name(x),.file_name(x))
  .file_size(x)<-file_info$`content-length`
  .file_type(x)<-file_info$`content-type`
  .URI(x)<-get_google_URI(.bucket_name(x),.file_name(x))
  .lastModified(x)<-file_info$`last-modified`
  x
}
#' Print object of class `FileClass`
#' 
#' @param object an object of class `FileClass`
#' 
#' @return Invisible `Object`
#' @export
setMethod("show", signature("FileClass"), function(object){
  cat("File:  ",.file_name(object),"\n")
  cat("Bucket:",.bucket_name(object),"\n")
  cat("Size:  ",printable_size(as.numeric(.file_size(object)))," \n")
  cat("Type:  ",.file_type(object),"\n")
  cat("URI:   ",.URI(object),"\n")
  cat("Last modified:",.lastModified(object),"\n")
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
setMethod("$",signature("FileClass"),function(x,name){
  if(!name%in%names(x)){
    stop("Cannot find the specific element `",name,"`")
  }
  if(name=="copy_to"){
    func1<-function(destination){
      gcs_cp(.URI(x), destination)
    }
    return(func1)
  }
  if(name == "delete"){
    func2 <-function(quiet = FALSE){
      if(!quiet){
        message("Do you want to delete the file? This operation cannot be undone[y/n]:")
        answer <- readline()
        if(tolower(answer)=="n") return()
      }
      delete_file(.bucket_name(x),.file_name(x))
    }
    return(func2)
  }
  if(name=="get_connection"){
    func3 <- function(open = "rb",encoding = getOption("encoding")){
      gcs_connection(description=.URI(x),open=open,encoding = encoding)
    }
    return(func3)
  }
  func <- get(paste0(".",name))
  func(x)
})
#' @param exact Logical(1), Controls possible partial matching
#' of `[[` when extracting by a character(1)
#' @rdname subset-FileClass-method
#' @export
setMethod("[[",signature("FileClass"),function(x,i,exact =TRUE){
  if(!exact){
    index <- pmatch(i,names(x))
    if(!is.na(index)){
      i=names(x)[index]
    }else{
      return(NULL)
    }
  }
  do.call("$",args=list(x=x,name=i))
})

#' @inherit base::names
#' @export
setMethod("names",signature("FileClass"),function(x){
  c("full_path","bucket_name","file_name","file_size"  , 
    "file_type","URI","lastModified",
    "copy_to","delete","get_connection")
})

