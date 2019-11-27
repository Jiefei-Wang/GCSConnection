## This file is for managing the python functions
## the variable python_env contains all exported python functions

python_env = new.env()
source_python('python/utils.py',envir= python_env)

get_input_stream<-function(credentials, bucket, file){
  stream <- python_env$get_input_stream(credentials,bucket,file)
  if(is.null(stream))
    stop("The file `",file, "` does not exist")
  list(stream, get_input_stream_size(stream))
}
get_output_stream<-function(credentials, bucket, file, content){
  stream <- python_env$get_output_stream(credentials,bucketName,
                                         file,content,gcs_get_output_stream_buff())
  python_env$open_output_stream(stream)
  list(stream)
}

close_output_stream <- function(stream){
  python_env$close_output_stream(stream)
}


get_input_stream_size <- function(stream){
  stream$size
}

read_stream<-function(stream,start = 0, end = 2 ^ .Machine$double.digits){
  python_env$read_stream(stream = stream,start = start, end = end)
}
write_stream<-function(stream, data){
  python_env$write_stream(stream,data)
}
