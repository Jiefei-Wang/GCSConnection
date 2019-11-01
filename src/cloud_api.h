//gcs::Client* client;
#ifndef NULL
#define NULL 0
#endif // !NULL



int setCredential(const char* cred);

void setProject(const char* project);

long long int getBucketNum();

long long int  getBucketNameSize(int index);

int getBucketName(int index, char* name);

bool isClientAvailable();

const char* getLastError();

