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


