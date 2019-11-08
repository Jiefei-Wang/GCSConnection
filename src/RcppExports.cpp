// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_initializeClient
void C_initializeClient(SEXP R_credential_name, SEXP R_project_name);
RcppExport SEXP _googleCloudStorage_C_initializeClient(SEXP R_credential_nameSEXP, SEXP R_project_nameSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_credential_name(R_credential_nameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_project_name(R_project_nameSEXP);
    C_initializeClient(R_credential_name, R_project_name);
    return R_NilValue;
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
// C_get_file_names
std::vector<std::string> C_get_file_names(SEXP bucket);
RcppExport SEXP _googleCloudStorage_C_get_file_names(SEXP bucketSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type bucket(bucketSEXP);
    rcpp_result_gen = Rcpp::wrap(C_get_file_names(bucket));
    return rcpp_result_gen;
END_RCPP
}
// getbucketConnectionCPP
SEXP getbucketConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file, SEXP canRead, SEXP canWrite, SEXP text, SEXP UTF8);
RcppExport SEXP _googleCloudStorage_getbucketConnectionCPP(SEXP credentialsSEXP, SEXP projectSEXP, SEXP bucketSEXP, SEXP fileSEXP, SEXP canReadSEXP, SEXP canWriteSEXP, SEXP textSEXP, SEXP UTF8SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type credentials(credentialsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type project(projectSEXP);
    Rcpp::traits::input_parameter< SEXP >::type bucket(bucketSEXP);
    Rcpp::traits::input_parameter< SEXP >::type file(fileSEXP);
    Rcpp::traits::input_parameter< SEXP >::type canRead(canReadSEXP);
    Rcpp::traits::input_parameter< SEXP >::type canWrite(canWriteSEXP);
    Rcpp::traits::input_parameter< SEXP >::type text(textSEXP);
    Rcpp::traits::input_parameter< SEXP >::type UTF8(UTF8SEXP);
    rcpp_result_gen = Rcpp::wrap(getbucketConnectionCPP(credentials, project, bucket, file, canRead, canWrite, text, UTF8));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_googleCloudStorage_C_initializeClient", (DL_FUNC) &_googleCloudStorage_C_initializeClient, 2},
    {"_googleCloudStorage_C_get_bucket_names", (DL_FUNC) &_googleCloudStorage_C_get_bucket_names, 0},
    {"_googleCloudStorage_C_get_file_names", (DL_FUNC) &_googleCloudStorage_C_get_file_names, 1},
    {"_googleCloudStorage_getbucketConnectionCPP", (DL_FUNC) &_googleCloudStorage_getbucketConnectionCPP, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_googleCloudStorage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
