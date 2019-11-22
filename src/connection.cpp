#include <Rcpp.h>
#include <string>
#include "macro.h"
#include "utils.h"

#define class myclass
#define private myprivate
#include "R_ext/Connections.h"
#undef class
#undef private

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
	SEXP blob;
};



static Rboolean open_connection(Rconnection con) {
	//Rprintf("file open\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (con->canread == TRUE) {
		List result = make_call("get_input_stream", wrap(bc->credentials), wrap(bc->bucket_name), wrap(bc->file_name));
		bc->connection_id = as<double>(result[0]);
		bc->blob = result[1];
		bc->file_size = as<double>(result[2]);
		con->incomplete = bc->file_size != 0 ? TRUE : FALSE;
		con->EOF_signalled = bc->file_size == 0 ? TRUE : FALSE;
	}
	if (con->canwrite == TRUE) {
		List result = make_call("get_output_stream", wrap(bc->credentials), wrap(bc->bucket_name), wrap(bc->file_name));
		bc->connection_id = as<double>(result[0]);
		bc->blob = result[1];
		bc->file_size = 0;
	}
	con->isopen = TRUE;
	return TRUE;

}

static void close_connection(Rconnection con) {
	Rprintf("file close\n");
}

static void destroy_connection(Rconnection con) {
	Rprintf("file destroy\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (con->canread == TRUE) {
		//Rprintf("delete buff:%p", con->buff);
		//free(con->buff);
	}
	if (con->canwrite == TRUE) {
		make_call("close_output_stream", bc->blob);
	}
	make_call("remove_blob", wrap(bc->connection_id));
}


static size_t read_connection(void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//Rprintf("%lld, %lld, %s\n", bc->offset, bc->file_size, con->EOF_signalled == TRUE ? "t" : "f");
	if (con->EOF_signalled == TRUE) return 0;
	size_t start = bc->offset;
	size_t request_size = size * nitems;
	size_t catched_size = con->buff_stored_len - con->buff_pos;


	size_t read_size;
	if (catched_size > request_size) {
		//Try to read from buff
		memcpy(target, con->buff + con->buff_pos, request_size);
		con->buff_pos = con->buff_pos + request_size;
		read_size = request_size;
	}
	else {
		//If there is no enough data in the buff
		//Read from the cloud

		size_t cloud_request_size = request_size - catched_size;
		//The size that will be read from the cloud, the extra data will be put in the buff
		size_t cloud_buff_size = cloud_request_size / con->buff_len * con->buff_len;
		cloud_buff_size = cloud_buff_size >= cloud_request_size ? cloud_buff_size : (cloud_buff_size + con->buff_len);
		SEXP result = Rf_protect(make_call("read_stream", bc->blob, wrap(start), wrap(start + cloud_buff_size - 1)));
		size_t cloud_read_size = XLENGTH(result);

		memcpy(target, con->buff + con->buff_pos, catched_size);
		read_size = catched_size;
		con->buff_pos = 0;
		con->buff_stored_len = 0;
		if (cloud_read_size > cloud_request_size) {
			read_size = read_size + cloud_request_size;
			memcpy((char*)target + catched_size, DATAPTR(result), cloud_request_size);
			memcpy(con->buff, (char*)DATAPTR(result)+ cloud_request_size, cloud_read_size - cloud_request_size);
			con->buff_pos = 0;
			con->buff_stored_len = cloud_read_size - cloud_request_size;
		}
		else {
			read_size = read_size + cloud_read_size;
			memcpy((char*)target + catched_size, DATAPTR(result), cloud_read_size);
		}
		Rf_unprotect(1);
	}
	bc->offset = bc->offset + read_size;
	con->incomplete = bc->offset < bc->file_size ? TRUE : FALSE;
	con->EOF_signalled = con->incomplete == TRUE ? FALSE : TRUE;
	//Rprintf("%lld, %lld, %s\n", bc->offset, bc->file_size, con->EOF_signalled == TRUE ? "t" : "f");
	return read_size;
}

static size_t write_connection(const void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t requestSize = size * nitems;
	SEXP tempVar = Rf_protect(Rf_allocVector(RAWSXP, requestSize));
	memcpy(DATAPTR(tempVar), target, requestSize);
	Rf_protect(make_call("write_stream", bc->blob, tempVar));
	Rprintf("file write:%lld bytes\n", requestSize);
	//bc->writeCon.write((const char*)target, requestSize);
	bc->offset = bc->offset + requestSize;
	Rf_unprotect(1);
	return requestSize;
}


static int get_byte_from_connection(Rconnection con) {
	int x;
	return read_connection(&x, 1, 1, con) ? x : R_EOF;
}


static double seek_connection(Rconnection con, double where, int origin, int rw) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	//printf("%d\n", rw);
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
	bool isRead, bool istext, bool UTF8, bool autoOpen, size_t buffLength) {
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
	SEXP rc = PROTECT(R_new_custom_connection(file.c_str(), openMode.c_str(), CONNECTION_CLASS, &con));


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
	con->close = close_connection;
	con->destroy = destroy_connection;
	con->read = read_connection;
	con->write = write_connection;
	con->fgetc = get_byte_from_connection;
	con->fgetc_internal = get_byte_from_connection;
	con->seek = seek_connection;
	//Only read has a buff
	if (isRead) {
		con->buff = (unsigned char*) malloc(buffLength);
		con->buff_len = buffLength;
		con->buff_pos = 0;
		con->buff_stored_len = 0;
	}


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


