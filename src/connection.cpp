#include "connection.h"
#include "R_ext/Connections.h"
#include <Rcpp.h>
#include "macro.h"
using namespace google::cloud;
/*
struct bucketCon {
	std::string credentials;
	std::string projectName;
	std::string bucketName;
	std::string fileName;

	gcs::Client* client = NULL;

	gcs::ObjectReadStream readCon;
	gcs::ObjectWriteStream writeCon;
};
*/



SEXP getBucketConnection(SEXP credentials, SEXP project, SEXP bucket, SEXP file) {
	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(TOCHAR(file), "rw", "googleBucket", &con));




	bucketConnection bc = new bucketCon;


	bc->projectName = TOCHAR(project);
	bc->bucketName = TOCHAR(bucket);
	bc->fileName = TOCHAR(file);
	bc->credentials = TOCHAR(credentials);
	auto creds = gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(bc->credentials);
	if (!creds) {
		Rf_error(creds.status().message().c_str());
	}
	bc->client = new gcs::Client(gcs::ClientOptions(*creds));

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

}




static Rboolean bucket_open(Rconnection con) {
	bucketConnection bc = (bucketConnection)con->private;

	bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());

	con->isopen = TRUE;
	con->incomplete = TRUE;
	return TRUE;
}

void bucket_close(Rconnection con) {
	bucketConnection bc = (bucketConnection)con->private;
	
	bc->readCon.Close();
	bc->writeCon.Close();

	con->isopen = FALSE;
	con->incomplete = FALSE;
}

void bucket_destroy(Rconnection con) {
	bucketConnection bc = (bucketConnection)con->private;
	delete bc->client;
}
template<class T>
size_t getStreamSize(T s) {
	size_t off = s->tellg();
	s->seekg(0, s->end);
	size_t length = s->tellg()- off;
	s->seekg(0, off);
	return length;
}


size_t bucket_read(void* target, size_t sz, size_t ni, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->private;
	size_t rest_len = getStreamSize(bc);
	size_t req_size = sz * ni;
	size_t read_size = rest_len > req_size ? req_size : rest_len;
	std::istream status = bc->readCon.read((char *)target, read_size);
	bc->writeCon.seekp(bc->readCon.tellg());

	con->incomplete = read_size != rest_len ? TRUE:FALSE;
	return read_size;
}

size_t bucket_write(const void* target, size_t sz, size_t ni, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->private;
	size_t req_size = sz * ni;
	bc->writeCon.write((const char *)target, req_size);
	StatusOr<gcs::ObjectMetadata> metadata = bc->writeCon.metadata();
	if (!metadata) {
		Rf_error(metadata.status().message().c_str());
	}
	return req_size;
}
