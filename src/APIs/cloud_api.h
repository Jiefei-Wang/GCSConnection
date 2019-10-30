//gcs::Client* client;
#define DLLEXPORT extern "C" 
#ifndef NULL
#define NULL 0
#endif // !NULL

DLLEXPORT
int __stdcall initialClient(const char* project, const char* creds = NULL);
DLLEXPORT
void __stdcall setProject(const char* project);
DLLEXPORT
long long int __stdcall getBucketNum();
DLLEXPORT
long long int __stdcall getBucketNameSize(int index);
DLLEXPORT
int __stdcall getBucketName(int index, char* name);
DLLEXPORT
bool __stdcall isClientAvailable();
DLLEXPORT
const char* __stdcall getLastError();

