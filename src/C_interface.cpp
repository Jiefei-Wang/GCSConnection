#include <Rcpp.h>
#include "google/cloud/storage/client.h"
#include "macro.h"
#include <vector>
#include <string>

namespace gcs = google::cloud::storage;
using namespace google::cloud;

gcs::Client* client;

// [[Rcpp::export]]
void C_initializeClient(SEXP R_credential_name,SEXP R_project_name){
  std::string credential_name=CHAR(Rf_asChar(R_credential_name));
  std::string project=CHAR(Rf_asChar(R_project_name));
  auto creds =
    gcs::oauth2::CreateServiceAccountCredentialsFromJsonFilePath(credential_name);
  gcs::ClientOptions opt(*creds);
  opt.set_project_id(project);
  client= new gcs::Client(opt);
}


// [[Rcpp::export]]
std::vector<std::string> C_get_bucket_names(){
  gcs::ListBucketsReader bucket_list = client->ListBuckets();
  std::vector<std::string> bucket_names;
  for (auto&& bucket_meta:bucket_list) {
    if (!bucket_meta) {
      Rf_error(bucket_meta.status().message().c_str());
    }
    bucket_names.push_back(bucket_meta->name());
  }
  return bucket_names;
}

// [[Rcpp::export]]
std::vector<std::string> C_get_file_names(SEXP bucket){
  std::string bucket_name = CHAR(Rf_asChar(bucket));
  gcs::ListObjectsReader file_list = client->ListObjects(bucket_name);
  
  std::vector<std::string> file_names;
  for (auto&& object_metadata : file_list) {
    if (!object_metadata) {
      Rf_error(object_metadata.status().message().c_str());
    }
    file_names.push_back(object_metadata->name());
  }
  return file_names;
}




extern "C" SEXP getbucketConnection(SEXP credentials, SEXP project, SEXP bucket, SEXP file, SEXP canRead, SEXP canWrite, SEXP text, SEXP UTF8);

// [[Rcpp::export]]
SEXP getbucketConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file,SEXP canRead, SEXP canWrite, SEXP text, SEXP UTF8){
  return getbucketConnection(credentials,project, bucket,file, canRead, canWrite, text,UTF8);
}

