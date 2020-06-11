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
	SEXP file_url;
	SEXP signed_url;
	size_t file_size;
	size_t offset;
};


static size_t write_connection_internal(void* target, size_t size, Rconnection con, bool final = false);


static Rboolean open_connection(Rconnection con) {
	//Rprintf("file open\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (con->canread == TRUE) {
		bc->file_size = as<size_t>(make_call("get_file_size", bc->file_url));
		bc->offset = 0;
		con->incomplete = bc->file_size != 0 ? TRUE : FALSE;
		con->EOF_signalled = bc->file_size == 0 ? TRUE : FALSE;
	}
	if (con->canwrite == TRUE) {
		bc->file_size = 0;
		if (bc->signed_url == R_NilValue) {
			bc->signed_url = make_call("start_upload", bc->file_url,
				wrap(con->text == TRUE ? "text/plain" : "application/octet-stream")
			);
			R_PreserveObject(bc->signed_url);
		}
	}
	con->isopen = TRUE;
	return TRUE;
}


static void destroy_connection(Rconnection con) {
	//Rprintf("file destroy\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Wired behavior: No need to free the buffer since R will free it
	//free(con->buff);

	if (con->canwrite == TRUE) {
		write_connection_internal(con->buff, con->buff_stored_len, con, true);
		make_call("stop_upload", bc->signed_url, wrap(bc->offset));
		R_ReleaseObject(bc->signed_url);
	}
	R_ReleaseObject(bc->file_url);

}


static size_t read_connection(void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("first: %lld, %lld, %lld, %s\n", bc->offset, bc->file_size, size * nitems,con->EOF_signalled == TRUE ? "t" : "f");
	if (con->EOF_signalled == TRUE) return 0;
	size_t request_size = size * nitems;
	size_t catched_size = con->buff_stored_len - con->buff_pos;
	//Rprintf("buffer length: %lld\n", con->buff_len);
	//Rprintf("buff_pos:%lld\n", con->buff_pos);
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
		size_t cloud_buff_size = cloud_request_size + con->buff_len;
		//Check if the read is in the end of the file
		cloud_buff_size = bc->file_size - bc->offset > cloud_buff_size ? cloud_buff_size : bc->file_size - bc->offset;
		//Rprintf("cloud_buff_size: %lld\n", cloud_buff_size);
		if (cloud_buff_size > 0) {
			SEXP result = Rf_protect(make_call("download_data", bc->file_url, wrap(bc->offset), wrap(bc->offset + cloud_buff_size - 1)));
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


static size_t write_connection_internal(void* target, size_t size, Rconnection con, bool final) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("begin file write:%lld bytes, off : %lld\n", size, bc->offset);
	SEXP tempVar;
	if (size > 0) {
		tempVar = Rf_protect(make_alt_raw(size, const_cast<void*>(target)));
	}
	else {
		if (final)
			tempVar = R_NilValue;
		else
			return 0;
	}
	make_call("upload_data", bc->signed_url, tempVar, wrap(bc->offset), wrap(bc->offset + size - 1), wrap(final));
	bc->offset = bc->offset + size;
	//Rprintf("finish file write:%lld bytes, off : %lld\n", size, bc->offset);
	if(size > 0)
		Rf_unprotect(1);
	return size;
}


static size_t write_connection(const void* target, size_t size, size_t nitems, Rconnection con) {
	size_t request_size = size * nitems;
	size_t buffer_space = con->buff_len - con->buff_stored_len;
	//Rprintf("request size:%lld\n", request_size);
	if (buffer_space > request_size) {
		memcpy((char*)con->buff + con->buff_stored_len, target, request_size);
		con->buff_stored_len = con->buff_stored_len + request_size;
		//Rprintf("write to buff, write size: %lld, stored length:%lld\n", request_size, con->buff_stored_len);
	}
	else {
		size_t offset = 0;
		while (request_size - offset >= buffer_space) {
			size_t empty_buff_size = con->buff_len - con->buff_stored_len;
			memcpy((char*)con->buff + con->buff_stored_len, (char*)target + offset, empty_buff_size);
			offset = offset + empty_buff_size;
			write_connection_internal(con->buff, con->buff_len, con);
			con->buff_stored_len = 0;
			buffer_space = con->buff_len;
			//Rprintf("write to buff and upload, write size: %lld, offset: %lld\n", empty_buff_size, offset);
		}
		memcpy(con->buff, (char*)target + offset, request_size - offset);
		con->buff_stored_len = request_size - offset;
	}

	return size * nitems;
}


static int get_byte_from_connection(Rconnection con) {
	char x;
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
SEXP get_bucket_connection(std::string bucket, std::string file,
	bool isRead, bool istext, bool UTF8,
	bool autoOpen, double buffLength,
	string description, string openMode) {

	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(description.c_str(),
		openMode.c_str(), CONNECTION_CLASS, &con));

	bucketConnection bc = new bucketCon();
	bc->offset = 0;
	bc->signed_url = R_NilValue;
	if (isRead) {
		bc->file_url = make_call("xml_url", wrap(bucket), wrap(file));
	}
	else {
		bc->file_url = make_call("json_upload_url", wrap(bucket), wrap(file));
	}
	R_PreserveObject(bc->file_url);

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
	con->buff = (unsigned char*)malloc(con->buff_len);
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


