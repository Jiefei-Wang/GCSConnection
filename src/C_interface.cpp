#include <Rcpp.h>
#include "cloud_api.h"
#include "macro.h"

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

