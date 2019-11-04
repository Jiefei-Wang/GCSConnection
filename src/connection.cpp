#include "connection.h"
#include "google/cloud/storage/client.h"
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
#include "connection.h"


#include <string>
#include "google/cloud/storage/client.h"

namespace gcs = google::cloud::storage;
typedef struct bucketCon* bucketConnection;

struct bucketCon {
	std::string credentials;
	std::string projectName;
	std::string bucketName;
	std::string fileName;

	gcs::Client* client = NULL;

	gcs::ObjectReadStream readCon;
	gcs::ObjectWriteStream writeCon;
};




void* createBuckekConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file) {
	bucketConnection bc = new bucketCon;
	bc->projectName = TOCHAR(project);
	bc->bucketName = TOCHAR(bucket);
	bc->fileName = TOCHAR(file);
	bc->credentials = TOCHAR(credentials);
	auto creds = gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(bc->credentials);
	if (!creds) {
		Rf_error(creds.status().message().c_str());
	}
	auto clientOptions = gcs::ClientOptions(*creds);
	clientOptions.set_project_id(bc->projectName);
	bc->client = new gcs::Client(clientOptions);
	return bc;
}


void openBucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	//bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());
}


void closeBucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	bc->readCon.Close();
	//bc->writeCon.Close();
}


void destropBucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	delete bc->client;
}


size_t readBucketConnectionCPP(void* target, size_t sz, size_t ni, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = sz * ni;
	bc->readCon.read((char*)target, req_size);
	size_t read_size = bc-> readCon.gcount();
	return read_size;
}

size_t writeBucketConnectionCPP(const void* target, size_t sz, size_t ni, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = sz * ni;
	bc->writeCon.write((const char*)target, req_size);
	StatusOr<gcs::ObjectMetadata> metadata = bc->writeCon.metadata();
	if (!metadata) {
		Rf_error(metadata.status().message().c_str());
	}
	return req_size;
}
