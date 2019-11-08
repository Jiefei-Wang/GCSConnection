#include "connection.h"
#include "google/cloud/storage/client.h"
#include "macro.h"
#include <string>
//#define size_t unsigned long long int


using namespace google::cloud;



namespace gcs = google::cloud::storage;
typedef struct bucketCon* bucketConnection;


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
	bc->offset = 0;
	bc->canRead = canRead;
	bc->canWrite = canWrite;
	auto fileMeta = bc->client->GetObjectMetadata(bucket,file);
	if(!fileMeta){
	  bc->fileSize = 0;
	}
	bc->fileSize = fileMeta->size();
	
	return bc;
}


bool openbucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	if (bc->canRead)
		bc->readCon = bc->client->ReadObject(bc->bucketName.c_str(), bc->fileName.c_str());
	if (bc->canWrite)
		bc->writeCon = bc->client->WriteObject(bc->bucketName.c_str(), bc->fileName.c_str());
	return true;
}


bool closebucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	if (bc->canRead)
		bc->readCon.Close();
	if (bc->canWrite){
	  bc->writeCon.Close();
	  StatusOr<gcs::ObjectMetadata> metadata = bc->writeCon.metadata();
	  if (!metadata) {
	    Rf_warning("Error in the write connection: ",metadata.status().message().c_str());
	  }
	}
	return true;
}


void destropbucketConnectionCPP(void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
  if(bc->client!=NULL)
	  delete bc->client;
}


size_t readbucketConnectionCPP(void* target, size_t size, size_t nitems, void* cbc) {
  //Rprintf("reading %lld bytes",size*nitems);
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = size * nitems;
	bc->readCon.read((char*)target, req_size);
	size_t read_size = bc->readCon.gcount();
	bc->offset = bc->offset + read_size;
	return read_size;
}

size_t writebucketConnectionCPP(const void* target, size_t size, size_t nitems, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
	size_t req_size = size * nitems;
	bc->writeCon.write((const char*)target, req_size);
	bc->offset = bc->offset + req_size;
	return req_size;
}


double seekbucketConnectionCPP(double where, int origin, void* cbc) {
	bucketConnection bc = (bucketConnection)cbc;
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