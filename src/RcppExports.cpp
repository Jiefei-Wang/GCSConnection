// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_set_credential
void C_set_credential(SEXP R_cred);
RcppExport SEXP _googleCloudStorage_C_set_credential(SEXP R_credSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_cred(R_credSEXP);
    C_set_credential(R_cred);
    return R_NilValue;
END_RCPP
}
// C_set_project
void C_set_project(SEXP R_project_name);
RcppExport SEXP _googleCloudStorage_C_set_project(SEXP R_project_nameSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_project_name(R_project_nameSEXP);
    C_set_project(R_project_name);
    return R_NilValue;
END_RCPP
}
// C_get_bucket_number
double C_get_bucket_number();
RcppExport SEXP _googleCloudStorage_C_get_bucket_number() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(C_get_bucket_number());
    return rcpp_result_gen;
END_RCPP
}
// C_get_bucket_names
std::vector<std::string> C_get_bucket_names();
RcppExport SEXP _googleCloudStorage_C_get_bucket_names() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(C_get_bucket_names());
    return rcpp_result_gen;
END_RCPP
}
// getBucketConnectionCPP
SEXP getBucketConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file);
RcppExport SEXP _googleCloudStorage_getBucketConnectionCPP(SEXP credentialsSEXP, SEXP projectSEXP, SEXP bucketSEXP, SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type credentials(credentialsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type project(projectSEXP);
    Rcpp::traits::input_parameter< SEXP >::type bucket(bucketSEXP);
    Rcpp::traits::input_parameter< SEXP >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(getBucketConnectionCPP(credentials, project, bucket, file));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_googleCloudStorage_C_set_credential", (DL_FUNC) &_googleCloudStorage_C_set_credential, 1},
    {"_googleCloudStorage_C_set_project", (DL_FUNC) &_googleCloudStorage_C_set_project, 1},
    {"_googleCloudStorage_C_get_bucket_number", (DL_FUNC) &_googleCloudStorage_C_get_bucket_number, 0},
    {"_googleCloudStorage_C_get_bucket_names", (DL_FUNC) &_googleCloudStorage_C_get_bucket_names, 0},
    {"_googleCloudStorage_getBucketConnectionCPP", (DL_FUNC) &_googleCloudStorage_getBucketConnectionCPP, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_googleCloudStorage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
