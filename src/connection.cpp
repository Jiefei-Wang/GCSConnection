#include <Rcpp.h>
#include <string>
#include "macro.h"
#include "utils.h"
#include "altrep.h"

#define class myclass
#define private myprivate
#include "R_ext/Connections.h"
#undef class
#undef private

// R_CONNECTIONS_VERSION should be 1
EXPECT(R_CONNECTIONS_VERSION, 1);


using namespace Rcpp;
using std::string;




typedef struct bucketCon* bucketConnection;
#define R_EOF -1

struct bucketCon {
	double connection_id = -1;
	string credentials;
	//NOT USED
	string project_name;
	string bucket_name;
	string file_name;
	size_t file_size;
	size_t offset;
	SEXP stream = NULL;
};


static size_t write_connection_internal(void* target, size_t size, Rconnection con);


static Rboolean open_connection(Rconnection con) {
	//Rprintf("file open\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (con->canread == TRUE) {
		List result = make_call("get_input_stream", wrap(bc->credentials), 
			wrap(bc->bucket_name), wrap(bc->file_name));
		
		bc->stream = result[0];
		bc->file_size = as<double>(result[1]);
		R_PreserveObject(bc->stream);
		con->incomplete = bc->file_size != 0 ? TRUE : FALSE;
		con->EOF_signalled = bc->file_size == 0 ? TRUE : FALSE;
	}
	if (con->canwrite == TRUE) {
		List result = make_call("get_output_stream", wrap(bc->credentials), 
			wrap(bc->bucket_name), wrap(bc->file_name),
			wrap(con->text == TRUE ? "text/plain" : "application/octet-stream")
			);
		bc->stream = result[0];
		bc->file_size = 0;
		R_PreserveObject(bc->stream);
	}
	con->isopen = TRUE;
	return TRUE;

}


static void destroy_connection(Rconnection con) {
	//Rprintf("file destroy\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Wired behavior: No need to free the buffer since R will free it
	//free(con->buff);
	
	if (bc->stream != NULL) {
		if (con->canwrite == TRUE) {
			if (con->buff_stored_len > 0) {
				write_connection_internal(con->buff, con->buff_stored_len, con);
			}
			make_call("close_output_stream", bc->stream);
		}
		R_ReleaseObject(bc->stream);
	}
	
		
}


static size_t read_connection(void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("first: %lld, %lld, %lld, %s\n", bc->offset, bc->file_size, size * nitems,con->EOF_signalled == TRUE ? "t" : "f");
	if (con->EOF_signalled == TRUE) return 0;
	size_t request_size = size * nitems;
	size_t catched_size = con->buff_stored_len - con->buff_pos;
	//Rprintf("buffer length: %lld\n", con->buff_len);
	
	size_t read_size;
	if (catched_size >= request_size) {
		//Try to read from buff
		memcpy(target, con->buff + con->buff_pos, request_size);
		con->buff_pos = con->buff_pos + request_size;
		read_size = request_size;
		bc->offset = bc->offset + read_size;
		//Rprintf("read cache1: %lld, %lld\n", read_size, bc->offset);
	}
	else {
		//If there is no enough data in the buff
		//Read from the buff
		memcpy(target, con->buff + con->buff_pos, catched_size);
		read_size = catched_size;
		bc->offset = bc->offset + catched_size;
		//clean buff
		con->buff_pos = 0;
		con->buff_stored_len = 0;
		//Rprintf("read cache2: %lld, %lld\n", read_size, bc->offset);

		//Read from the cloud
		size_t cloud_request_size = request_size - catched_size;
		//Rprintf("cloud request size: %lld\n", cloud_request_size);
		//The size that will be read from the cloud, the extra data will be put in the buff
		size_t cloud_buff_size = cloud_request_size / con->buff_len * con->buff_len;
		cloud_buff_size = cloud_buff_size >= cloud_request_size ? cloud_buff_size : (cloud_buff_size + con->buff_len);
		//Check if the read is in the end of the file
		cloud_buff_size = bc->file_size - bc->offset > cloud_buff_size ? cloud_buff_size : bc->file_size - bc->offset;
		//Rprintf("cloud_buff_size: %lld\n", cloud_buff_size);
		if (cloud_buff_size > 0) {
			SEXP result = Rf_protect(make_call("read_stream", bc->stream, wrap(bc->offset), wrap(bc->offset + cloud_buff_size - 1)));
			size_t cloud_read_size = XLENGTH(result);
			//Rprintf("cloud_read_size: %lld\n", cloud_read_size);

			if (cloud_read_size > cloud_request_size) {
				/*
				If the read size is larger than the required size
				put the rest into the buffer
				*/
				read_size = read_size + cloud_request_size;
				con->buff_stored_len = cloud_read_size - cloud_request_size;

				memcpy((char*)target + catched_size, DATAPTR(result), cloud_request_size);
				memcpy(con->buff, (char*)DATAPTR(result) + cloud_request_size, con->buff_stored_len);

				bc->offset = bc->offset + cloud_request_size;
				//Rprintf("read cloud1: %lld,%lld , %lld\n", cloud_request_size, cloud_read_size, bc->offset);
				//Rprintf("cached size: %lld\n", con->buff_stored_len);
			}
			else {
				read_size = read_size + cloud_read_size;
				memcpy((char*)target + catched_size, DATAPTR(result), cloud_read_size);

				bc->offset = bc->offset + cloud_read_size;
				//Rprintf("read cloud2: %lld, %lld\n", cloud_read_size, bc->offset);
			}
			Rf_unprotect(1);
		}
	}
	con->incomplete = bc->offset < bc->file_size ? TRUE : FALSE;
	con->EOF_signalled = con->incomplete == TRUE ? FALSE : TRUE;
	//Rprintf("second: %lld, %lld, %s\n", bc->offset, bc->file_size, con->EOF_signalled == TRUE ? "t" : "f");
	return read_size;
}

static size_t write_connection_internal(void* target, size_t size, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("begin file write:%lld bytes, off : %lld\n", size, bc->offset);
	SEXP tempVar = Rf_protect(make_alt_raw(size, const_cast<void*>(target)));
	make_call("write_stream", bc->stream, tempVar);
	bc->offset = bc->offset + size;
	//Rprintf("finish file write:%lld bytes, off : %lld\n", size, bc->offset);
	Rf_unprotect(1);
	return size;
}

static size_t write_connection(const void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t request_size = size * nitems;
	size_t buffer_space = con->buff_len - con->buff_stored_len;

	if (buffer_space > request_size) {
		memcpy((char*)con->buff + con->buff_stored_len, target, request_size);
		con->buff_stored_len = con->buff_stored_len + request_size;
	}
	else {
		write_connection_internal(con->buff, con->buff_stored_len, con);
		write_connection_internal(const_cast<void*>(target), request_size, con);
		con->buff_stored_len = 0;
	}

	return size * nitems;
}





static int get_byte_from_connection(Rconnection con) {
	int x;
	return read_connection(&x, 1, 1, con) ? x : R_EOF;
}


static double seek_connection(Rconnection con, double where, int origin, int rw) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("%d\n", rw);
	if (!(rw == 0 && con->canread == TRUE))
		Rf_error("Only read connection is seekable");

	if (ISNA(where)) {
		return bc->offset;
	}

	size_t oldOffset = bc->offset;
	if (origin == 1) {
		//start
		bc->offset = where;
	}
	else if (origin == 2) {
		// current
		bc->offset = bc->offset + where;
	}
	else {
		// end
		bc->offset = bc->file_size + where;
	}
	if (bc->offset < bc->file_size) {
		con->incomplete = TRUE;
		con->EOF_signalled = FALSE;
	}
	return oldOffset;
}


// [[Rcpp::export]]
SEXP get_bucket_connection(std::string credentials, std::string project, std::string bucket, std::string file,
	bool isRead, bool istext, bool UTF8, bool autoOpen, double buffLength) {
	string openMode;
	if (isRead && istext) {
		openMode = "r";
	}
	else if (isRead && !istext) {
		openMode = "rb";
	}
	else if (!isRead && istext) {
		openMode = "w";
	}
	else if (!isRead && !istext) {
		openMode = "wb";
	}


	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(file.c_str(),
		openMode.c_str(), CONNECTION_CLASS, &con));

	bucketConnection bc = new bucketCon();
	bc->project_name = project;
	bc->bucket_name = bucket;
	bc->file_name = file;
	bc->credentials = credentials;
	bc->offset = 0;


	con->incomplete = FALSE;
	con->myprivate = bc;
	con->canseek = isRead ? TRUE : FALSE;
	con->canread = isRead ? TRUE : FALSE;
	con->canwrite = (!isRead) ? TRUE : FALSE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = istext ? TRUE : FALSE;
	con->UTF8out = UTF8 ? TRUE : FALSE;
	con->open = open_connection;
	//con->close = close_connection;
	con->destroy = destroy_connection;
	con->read = read_connection;
	con->write = write_connection;
	con->fgetc = get_byte_from_connection;
	con->fgetc_internal = get_byte_from_connection;
	con->seek = seek_connection;
	con->buff_len = buffLength;
	//No need to free the memory after usage.
	con->buff = (unsigned char*) malloc(con->buff_len);
	con->buff_pos = 0;
	con->buff_stored_len = 0;


	if (autoOpen) {
		Rboolean success = con->open(con);
		if (!success) {
			con->destroy(con);
			Rf_error("cannot open the connection");
		}
	}
	UNPROTECT(1);
	return rc;
}


