#include "google/cloud/storage/client.h"
#include "cloud_api.h"
#include <string>

namespace gcs = google::cloud::storage;
using namespace google::cloud;

#define ERROR_CLIENT_NOT_AVAILABLE "Client is not available"



char * credentials = new char[512];
gcs::Client* client = NULL;
std::string* projectId = NULL;

std::string lastError;

int sendError(const std::string msg) {
  lastError = msg;
  return -1;
}


int setCredential(const char* cred){
  strcpy(credentials,cred);
  std::string path = std::string(credentials);
  auto creds =
    gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(path);
  if (!creds) {
    return sendError(creds.status().message());
  }
  if (client != NULL) delete client;
  client = new gcs::Client(gcs::ClientOptions(*creds));
  return 0;
}


void setProject(const char * project) {
  if (projectId != NULL) delete projectId;
  projectId = new std::string(project);
};



long long int getBucketNum() {
  if(!isClientAvailable()) return sendError(ERROR_CLIENT_NOT_AVAILABLE);
  auto bucket_list = client->ListBucketsForProject(*projectId);
  return std::distance(bucket_list.begin(), bucket_list.end());
}

long long int getBucketNameSize(int index) {
  if (!isClientAvailable()) return sendError(ERROR_CLIENT_NOT_AVAILABLE);
  auto bucket_list = client->ListBucketsForProject(*projectId);
  auto ith = std::next(bucket_list.begin(), index);
  
  if (!(*ith)) {
    return sendError((*ith).status().message());
  }
  return (*ith)->name().size();
}

int getBucketName(int index, char* name) {
  if (!isClientAvailable()) return sendError(ERROR_CLIENT_NOT_AVAILABLE);
  auto bucket_list = client->ListBucketsForProject(*projectId);
  auto ith = std::next(bucket_list.begin(), index);
  if (!(*ith)) {
    return sendError((*ith).status().message());
  }
  strcpy(name,(*ith)->name().c_str());
  return 0;
}



bool isClientAvailable() {
  return client != NULL;
}

const char* getLastError() {
  return lastError.c_str();
}