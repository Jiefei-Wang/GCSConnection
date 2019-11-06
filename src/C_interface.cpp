#include <Rcpp.h>
#include "cloud_api.h"
#include "macro.h"
#include <vector>
#include <string>

// [[Rcpp::export]]
void C_set_credential(SEXP R_cred){
  const char *cred=CHAR(Rf_asChar(R_cred));
  setCredential(cred);
}

// [[Rcpp::export]]
void C_set_project(SEXP R_project_name){
  const char *project_name=CHAR(Rf_asChar(R_project_name));
  setProject(project_name);
}

// [[Rcpp::export]]
double C_get_bucket_number(){
  double num = getBucketNum();
  CHECK_ERROR(num);
  return(num);
}

// [[Rcpp::export]]
std::vector<std::string> C_get_bucket_names(){
  double num = getBucketNum();
  CHECK_ERROR(num);
  char * name = (char *)malloc(512*sizeof(char));
  std::vector<std::string> v;
  for(int i=0;i<num;i++){
    getBucketName(i,name);
    v.push_back(std::string(name));
  }
  free(name);
  return(v);
}

extern "C" SEXP getbucketConnection(SEXP credentials, SEXP project, SEXP bucket, SEXP file£¬SEXP canRead, SEXP canWrite);

// [[Rcpp::export]]
SEXP getbucketConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file£¬SEXP canRead, SEXP canWrite, SEXP text, SEXP UTF8){
  return getbucketConnection(credentials,project, bucket,file, canRead, canWrite, text,UTF8);
}

