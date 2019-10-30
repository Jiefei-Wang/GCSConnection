#include <Rcpp.h>
#include "APIs/cloud_api.h"
using namespace Rcpp;


/*
 int initialClient(const char * project, char * creds = nullptr);
 void setProject(const char * project);
 size_t getBucketNum();
 size_t getBucketNameSize(int index);
 int getBucketName(int index, char * name);
 bool isClientAvailable();
 const char* getLastError();
*/


// [[Rcpp::export]]
int C_initialClient(SEXP R_project, SEXP R_creds){
  const char * project = CHAR(STRING_ELT(R_project, 0));
  const char * creds = CHAR(STRING_ELT(R_creds, 0));

  return initialClient(project,creds);
}