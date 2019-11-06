#include "connection.h"
#include "google/cloud/storage/client.h"
#include "macro.h"
using namespace google::cloud;

#include "connection.h"
#include <string>

namespace gcs = google::cloud::storage;
typedef struct bucketCon* bucketConnection;

struct bucketCon {
	std::string credentials;
	std::string projectName;
	std::string bucketName;
	std::string fileName;
	gcs::Client* client = NULL;
	size_t offset;
	bool canRead;
	bool canWrite;
	gcs::ObjectReadStream readCon;
	gcs::ObjectWriteStream writeCon;
};




void* createBuckekConnectionCPP(const char* credentials, const char* project, const char* bucket, const char* file, bool canRead, bool canWrite) {
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
	bc->offset;
	bc->canRead = canRead;
	bc->canWrite = canWrite;
	return bc;
}


void openbucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	if(bc->canRead)
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	if (bc->canWrite)
		bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());
}


void closebucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	if (bc->canRead)
		bc->readCon.Close();
	if (bc->canWrite)
		bc->writeCon.Close();
}


void destropbucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	delete bc->client;
}


size_t readbucketConnectionCPP(void* target, size_t size, size_t nitems, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = size * nitems;
	bc->readCon.read((char*)target, req_size);
	size_t read_size = bc-> readCon.gcount();
	return read_size;
}

size_t writebucketConnectionCPP(const void* target, size_t size, size_t nitems, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = size * nitems;
	bc->writeCon.write((const char*)target, req_size);
	StatusOr<gcs::ObjectMetadata> metadata = bc->writeCon.metadata();
	if (!metadata) {
		Rf_error(metadata.status().message().c_str());
	}
	return req_size;
}
