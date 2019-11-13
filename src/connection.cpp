#include <Rcpp.h>
#include "google/cloud/storage/client.h"
#include "macro.h"
#include <string>
#define class myclass
#define private myprivate
#include "R_ext/Connections.h"
#undef class
#undef private


using namespace google::cloud;
using std::string;

namespace gcs = google::cloud::storage;

typedef struct bucketCon* bucketConnection;
#define R_EOF -1


struct bucketCon {
	std::string credentials;
	std::string projectName;
	std::string bucketName;
	std::string fileName;
	gcs::Client* client = NULL;
	size_t fileSize;
	size_t offset;
	bool canRead;
	bool canWrite;
	gcs::ObjectReadStream readCon;
	gcs::ObjectWriteStream writeCon;
};

static Rboolean bucket_open(Rconnection con) {
	//Rprintf("file open\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (bc->canRead)
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	if (bc->canWrite)
		bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());

	con->isopen = TRUE;
	con->incomplete = TRUE;
	return TRUE;
}

void bucket_close(Rconnection con) {
	//Rprintf("file close\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (bc->canRead)
		bc->readCon.Close();
	if (bc->canWrite) {
		bc->writeCon.Close();
		StatusOr<gcs::ObjectMetadata> metadata = bc->writeCon.metadata();
		if (!metadata) {
			Rf_warning("Error in the write connection: ", metadata.status().message().c_str());
		}
	}
		con->isopen = FALSE;
		con->incomplete = FALSE;
}

void bucket_destroy(Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (bc->client != NULL)
		delete bc->client;
	con->isopen = FALSE;
	con->incomplete = FALSE;
}


size_t bucket_read(void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t req_size = size * nitems;
	bc->readCon.read((char*)target, req_size);
	size_t read_size = bc->readCon.gcount();
	bc->offset = bc->offset + read_size;


	con->incomplete = read_size != size * nitems ? TRUE : FALSE;
	return read_size;
}

size_t bucket_write(const void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t req_size = size * nitems;
	bc->writeCon.write((const char*)target, req_size);
	bc->offset = bc->offset + req_size;
	return req_size;
}


int bucket_fgetc(Rconnection con) {
	int x;
	return bucket_read(&x, 1, 1, con) ? x : R_EOF;
}


double seekbucketConnection(Rconnection con, double where, int origin, int rw) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (rw == 2)
		Rf_error("write connection is not seekable");
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
		//TODO: implement length
		bc->offset = bc->fileSize + where;
	}

	if (bc->offset != oldOffset) {
		bc->readCon.Close();
	}
	if (!bc->readCon.IsOpen()) {
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str(), gcs::ReadRange(bc->offset, LLONG_MAX));
	}
	return oldOffset;
}


// [[Rcpp::export]]
SEXP getbucketConnection(std::string R_credentials, SEXP R_project, SEXP R_bucket, SEXP R_file,
	SEXP R_canRead,
	SEXP R_canWrite, SEXP R_text, SEXP R_UTF8, SEXP open) {

	char model[3];
	bool canRead = Rf_asLogical(R_canRead);
	bool canWrite = Rf_asLogical(R_canWrite);
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
	//void* bc = createBuckekConnectionCPP(TOCHAR(R_credentials), TOCHAR(R_project), TOCHAR(R_bucket), TOCHAR(R_file), canRead, canWrite);

	bucketConnection bc = new bucketCon();
	bc->projectName = TOCHAR(R_project);
	bc->bucketName = TOCHAR(R_bucket);
	bc->fileName = TOCHAR(R_file);
	bc->credentials = R_credentials.c_str();
	auto creds = gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(bc->credentials);
	if (!creds) {
		Rf_error(creds.status().message().c_str());
	}
	auto clientOptions = gcs::ClientOptions(*creds);
	clientOptions.set_project_id(bc->projectName);
	bc->client = new gcs::Client(clientOptions);
	bc->offset = 0;
	bc->canRead = canRead;
	bc->canWrite = canWrite;

	auto fileMeta = bc->client->GetObjectMetadata(TOCHAR(R_bucket), TOCHAR(R_file));
	if (!fileMeta) {
		bc->fileSize = 0;
	}
	bc->fileSize = fileMeta->size();




	con->incomplete = FALSE;
	con->myprivate = bc;
	con->canseek = canRead ? TRUE : FALSE;
	con->canread = canRead ? TRUE : FALSE;
	con->canwrite = canWrite ? TRUE : FALSE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = Rf_asLogical(R_text) ? TRUE : FALSE;
	con->UTF8out = Rf_asLogical(R_UTF8) ? TRUE : FALSE;
	con->open = bucket_open;
	con->close = bucket_close;
	con->destroy = bucket_destroy;
	con->read = bucket_read;
	con->write = bucket_write;
	con->fgetc = bucket_fgetc;
	con->fgetc_internal = bucket_fgetc;
	con->seek = seekbucketConnection;
	if (Rf_asLogical(open)) {
		Rboolean success = con->open(con);
		if (!success) {
			con->destroy(con);
			Rf_error("cannot open the connection");
		}
	}
	UNPROTECT(1);
	return rc;
}


