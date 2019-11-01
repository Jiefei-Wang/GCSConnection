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

static Rboolean bucket_open(Rconnection con);
void bucket_close(Rconnection con);
void bucket_destroy(Rconnection con);
size_t bucket_read(void* target, size_t sz, size_t ni, Rconnection con);
size_t bucket_write(const void* target, size_t sz, size_t ni, Rconnection con);


