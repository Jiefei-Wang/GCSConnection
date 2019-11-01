#include "R_ext/Connections.h"
#include "Rinternals.h"
#include "macro.h"
#include "connection.h"

static Rboolean bucket_open(Rconnection con);
void bucket_close(Rconnection con);
void bucket_destroy(Rconnection con);
size_t bucket_read(void* target, size_t sz, size_t ni, Rconnection con);
size_t bucket_write(const void* target, size_t sz, size_t ni, Rconnection con);

// [[Rcpp::export]]
SEXP getBucketConnection(SEXP credentials, SEXP project, SEXP bucket, SEXP file) {
	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(TOCHAR(file), "rw", "googleBucket", &con));
	void* bc = createBuckekConnectionCPP(credentials, project, bucket, file);
	
	con->incomplete = FALSE;
	con->private = bc;
	con->canseek = FALSE;
	con->canwrite = TRUE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = TRUE;
	con->UTF8out = TRUE;
	con->open = bucket_open;
	con->close = bucket_close;
	con->destroy = bucket_destroy;
	con->read = bucket_read;
	con->write = bucket_write;
	UNPROTECT(1);
	return rc;
}




static Rboolean bucket_open(Rconnection con) {
	void* bc = con->private;
	openBucketConnectionCPP(bc);


	con->isopen = TRUE;
	con->incomplete = TRUE;
	return TRUE;
}

void bucket_close(Rconnection con) {
	void* bc = con->private;
	closeBucketConnectionCPP(bc);
	

	con->isopen = FALSE;
	con->incomplete = FALSE;
}

void bucket_destroy(Rconnection con) {
	void* bc = con->private;
	destropBucketConnectionCPP(bc);
}



size_t bucket_read(void* target, size_t sz, size_t ni, Rconnection con) {
	void* bc = con->private;
	
	size_t read_size = readBucketConnectionCPP(target, sz, ni, bc);
	con->incomplete = read_size != sz*ni? TRUE : FALSE;
	return read_size;
}

size_t bucket_write(const void* target, size_t sz, size_t ni, Rconnection con) {
	void* bc = con->private;
	size_t req_size = writeBucketConnectionCPP(target, sz, ni, bc);
	return req_size;
}
