#include "Rinternals.h"
#include "R_ext/Connections.h"
#include "macro.h"
#include "connection.h"
#include <stdbool.h>
#include <string.h>

#define R_EOF -1


<<<<<<< HEAD
static Rboolean bucket_open(Rconnection con);
void bucket_close(Rconnection con);
void bucket_destroy(Rconnection con);
size_t bucket_read(void* target, size_t sz, size_t ni, Rconnection con);
size_t bucket_write(const void* target, size_t sz, size_t ni, Rconnection con);
int bucket_fgetc(Rconnection con);



SEXP getbucketConnection(SEXP R_credentials, SEXP R_project, SEXP R_bucket, SEXP R_file, SEXP R_canRead, SEXP R_canWrite, SEXP R_text, SEXP R_UTF8) {

	char model[3];
	bool canRead = asLogical(R_canRead);
	bool canWrite = asLogical(R_canWrite);
	if(canRead){
		strcpy(model, "r");
	}
	else if (canWrite) {
		strcpy(model, "w");
	}else{
	  Rf_error("Unknow options");
	}
	
	if(!asLogical(R_text)){
	  strcpy(model+1, "b");
	}
	


	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(TOCHAR(R_file), model, "googleBucket", &con));
	void* bc = createBuckekConnectionCPP(TOCHAR(R_credentials), TOCHAR(R_project), TOCHAR(R_bucket), TOCHAR(R_file), canRead, canWrite);
	
	con->incomplete = FALSE;
	con->private = bc;
	con->canseek = FALSE;
	con->canread = canRead ? TRUE : FALSE;
	con->canwrite = canWrite ?TRUE:FALSE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = asLogical(R_text)?TRUE:FALSE;
	con->UTF8out = asLogical(R_UTF8) ? TRUE : FALSE;
	con->open = bucket_open;
	con->close = bucket_close;
	con->destroy = bucket_destroy;
	con->read = bucket_read;
	con->write = bucket_write;
	con->fgetc = bucket_fgetc;
	con->fgetc_internal = bucket_fgetc;
	UNPROTECT(1);
	return rc;
}



=======
>>>>>>> c6506b33ca3892b60d90a0cb7b9bddd2afd88646

static Rboolean bucket_open(Rconnection con) {
	void* bc = con->private;
	openbucketConnectionCPP(bc);


	con->isopen = TRUE;
	con->incomplete = TRUE;
	return TRUE;
}

void bucket_close(Rconnection con) {
	void* bc = con->private;
	closebucketConnectionCPP(bc);
	con->isopen = FALSE;
	con->incomplete = FALSE;
}

void bucket_destroy(Rconnection con) {
	void* bc = con->private;
	destropbucketConnectionCPP(bc);
	con->isopen = FALSE;
	con->incomplete = FALSE;
}


size_t bucket_read(void* target, size_t size, size_t nitems, Rconnection con) {
	void* bc = con->private;
	
	size_t read_size = readbucketConnectionCPP(target, size, nitems, bc);
	con->incomplete = read_size != size * nitems ? TRUE : FALSE;
	return read_size;
}

size_t bucket_write(const void* target, size_t size, size_t nitems, Rconnection con) {
	void* bc = con->private;
	size_t req_size = writebucketConnectionCPP(target, size, nitems, bc);
	return req_size;
}


int bucket_fgetc(Rconnection con) {
	int x;
	return bucket_read(&x, 1, 1, con)? x: R_EOF;
}


double seekbucketConnection(Rconnection con, double where, int origin, int rw) {
	void* bc = con->private;
	if (rw == 2)
		Rf_error("write connection is not seekable");
	return seekbucketConnectionCPP(where, origin, bc);
}



SEXP getbucketConnection(SEXP R_credentials, SEXP R_project, SEXP R_bucket, SEXP R_file, SEXP R_canRead, SEXP R_canWrite, SEXP R_text, SEXP R_UTF8) {

	char model[3];
	bool canRead = asLogical(R_canRead);
	bool canWrite = asLogical(R_canWrite);
	if (canRead && canWrite) {
		strcpy(model, "rw");
	}
	else if (canRead) {
		strcpy(model, "r");
	}
	else if (canWrite) {
		strcpy(model, "w");
	}


	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(TOCHAR(R_file), model, "googleBucket", &con));
	void* bc = createBuckekConnectionCPP(TOCHAR(R_credentials), TOCHAR(R_project), TOCHAR(R_bucket), TOCHAR(R_file), canRead, canWrite);

	con->incomplete = FALSE;
	con->private = bc;
	con->canseek = canRead ? TRUE : FALSE;
	con->canread = canRead ? TRUE : FALSE;
	con->canwrite = canWrite ? TRUE : FALSE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = asLogical(R_text) ? TRUE : FALSE;
	con->UTF8out = asLogical(R_UTF8) ? TRUE : FALSE;
	con->open = bucket_open;
	con->close = bucket_close;
	con->destroy = bucket_destroy;
	con->read = bucket_read;
	con->write = bucket_write;
	con->fgetc = bucket_fgetc;
	con->fgetc_internal = bucket_fgetc;
	con->seek = seekbucketConnection;
	UNPROTECT(1);
	return rc;
}

