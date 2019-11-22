## This file is for managing the python functions
## the variable python_env contains all exported python functions
## It also keeps track of the stream objects that are created by
## the package
python_env = new.env()
source_python('python/utils.py',envir= python_env)
python_env$blob_number <- 0
python_env$blob_list <- new.env()



get_blob_number <- function(){
  python_env$blob_number
}
set_blob_number <- function(x){
  python_env$blob_number <- x
}
add_blob<-function(id, blob){
  python_env$blob_list[[as.character(id)]]=blob
}
remove_blob<-function(id){
  remove(list= as.character(id),envir = python_env$blob_list)
}
get_input_stream<-function(credentials, bucket, file){
  blob <- python_env$get_input_stream(credentials,bucket,file)
  if(is.null(blob))
    stop("The file `",file, "` does not exist")
  set_blob_number(get_blob_number()+1)
  id <- get_blob_number()
  add_blob(id, blob)
  list(id, blob, get_input_stream_size(blob))
}
get_output_stream<-function(credentials, bucket, file){
  stream <- python_env$get_output_stream(creds,bucketName,file)
  python_env$open_output_stream(stream)
  set_blob_number(get_blob_number()+1)
  id <- get_blob_number()
  add_blob(id, stream)
  list(id, stream)
}

close_output_stream <- function(stream){
  python_env$close_output_stream(stream)
}


get_input_stream_size <- function(blob){
  blob$size
}

read_stream<-function(stream,start = 0, end = 2 ^ .Machine$double.digits){
  python_env$read_stream(stream = stream,start = start, end = end)
}
write_stream<-function(stream, data){
  python_env$write_stream(stream,data)
}
