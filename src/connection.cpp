#include <Rcpp.h>
#include "google/cloud/storage/client.h"
#include "macro.h"
#include <string>

#define class myclass
#define private myprivate
#include "R_ext/Connections.h"
#undef class
#undef private



namespace gcs = google::cloud::storage;
using namespace google::cloud;
using std::string;


typedef struct bucketCon* bucketConnection;
#define R_EOF -1


struct bucketCon {
	string credentials;
	string projectName;
	string bucketName;
	string fileName;
	gcs::Client* client = NULL;
	size_t fileSize;
	size_t offset;
	bool isRead;
	bool isWrite;
	gcs::ObjectReadStream readCon;
	gcs::ObjectWriteStream writeCon;
};

static void closeWriteConnection(gcs::ObjectWriteStream& writeCon) {
	if (writeCon.IsOpen()) {
		writeCon.Close();
		StatusOr<gcs::ObjectMetadata> metadata = writeCon.metadata();
		if (!metadata) {
			Rf_warning("Error in the write connection: %s", metadata.status().message().c_str());
		}
	}
}


static Rboolean openConnection(Rconnection con) {
	//Rprintf("file open\n");
	bucketConnection bc = (bucketConnection)con->myprivate;

	auto fileMeta = bc->client->GetObjectMetadata(bc->bucketName.c_str(), bc->fileName.c_str());
	if (!fileMeta) {
		bc->fileSize = 0;
	}
	bc->fileSize = fileMeta->size();

	if (bc->isRead)
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	if (bc->isWrite) {
		closeWriteConnection(bc->writeCon);
		bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());
	}
	
	con->isopen = TRUE;
	con->incomplete = TRUE;
	return TRUE;
}

void closeConnection(Rconnection con) {
	//Rprintf("file close\n");
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (bc->isRead)
		bc->readCon.Close();
	if (bc->isWrite) {
		closeWriteConnection(bc->writeCon);
	}
	con->isopen = FALSE;
	con->incomplete = FALSE;
}

void destroyConnection(Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (bc->client != NULL)
		delete bc->client;
	con->isopen = FALSE;
	con->incomplete = FALSE;
}


size_t readConnection(void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t requestSize = size * nitems;
	bc->readCon.read((char*)target, requestSize);
	size_t readSize = bc->readCon.gcount();
	bc->offset = bc->offset + readSize;
	con->incomplete = bc->offset != bc->fileSize ? TRUE : FALSE;
	return readSize;
}

size_t writeConnection(const void* target, size_t size, size_t nitems, Rconnection con) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	size_t requestSize = size * nitems;
	bc->writeCon.write((const char*)target, requestSize);
	bc->offset = bc->offset + requestSize;
	return requestSize;
}


int getCharFromConnection(Rconnection con) {
	int x;
	return readConnection(&x, 1, 1, con) ? x : R_EOF;
}


double seekConnection(Rconnection con, double where, int origin, int rw) {
	bucketConnection bc = (bucketConnection)con->myprivate;
	if (rw != 1|| !bc->isRead)
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
		bc->offset = bc->fileSize + where;
	}

	if (bc->offset != oldOffset) {
		bc->readCon.Close();
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str(), gcs::ReadRange(bc->offset, LLONG_MAX));
	}
	return oldOffset;
}


// [[Rcpp::export]]
SEXP getbucketConnection(std::string credentials, std::string project, std::string bucket, std::string file,
	bool isRead, bool istext, bool UTF8, bool autoOpen) {
	string openMode;
	if (isRead && istext) {
		openMode = "r";
	}
	else if (isRead && !istext) {
		openMode = "rb";
	}else if (!isRead && istext) {
		openMode = "w";
	}
	else if (!isRead && !istext) {
		openMode = "wb";
	}


	Rconnection con;
	SEXP rc = PROTECT(R_new_custom_connection(file.c_str(), openMode.c_str(), CONNECTION_CLASS, &con));

	bucketConnection bc = new bucketCon();
	bc->projectName = project;
	bc->bucketName = bucket;
	bc->fileName = file;
	bc->credentials = credentials;
	auto creds = gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(bc->credentials);
	if (!creds) {
		Rf_error(creds.status().message().c_str());
	}
	auto clientOptions = gcs::ClientOptions(*creds);
	clientOptions.set_project_id(bc->projectName);
	bc->client = new gcs::Client(clientOptions);
	bc->offset = 0;
	bc->isRead = isRead;
	bc->isWrite = !isRead;


	con->incomplete = FALSE;
	con->myprivate = bc;
	con->canseek = isRead ? TRUE : FALSE;
	con->canread = isRead ? TRUE : FALSE;
	con->canwrite = (!isRead) ? TRUE : FALSE;
	con->isopen = FALSE;
	con->blocking = TRUE;
	con->text = istext ? TRUE : FALSE;
	con->UTF8out = UTF8 ? TRUE : FALSE;
	con->open = openConnection;
	con->close = closeConnection;
	con->destroy = destroyConnection;
	con->read = readConnection;
	con->write = writeConnection;
	con->fgetc = getCharFromConnection;
	con->fgetc_internal = getCharFromConnection;
	con->seek = seekConnection;
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


