// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "GCSConnection_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// C_package_onLoad
void C_package_onLoad(SEXP pkg_namespace);
RcppExport SEXP _GCSConnection_C_package_onLoad(SEXP pkg_namespaceSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type pkg_namespace(pkg_namespaceSEXP);
    C_package_onLoad(pkg_namespace);
    return R_NilValue;
END_RCPP
}
// get_bucket_connection
SEXP get_bucket_connection(string bucket, string file, bool is_read, bool is_text, bool UTF8, bool auto_open, double buff_length, string description, string open_mode, SEXP billing_project);
RcppExport SEXP _GCSConnection_get_bucket_connection(SEXP bucketSEXP, SEXP fileSEXP, SEXP is_readSEXP, SEXP is_textSEXP, SEXP UTF8SEXP, SEXP auto_openSEXP, SEXP buff_lengthSEXP, SEXP descriptionSEXP, SEXP open_modeSEXP, SEXP billing_projectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< string >::type bucket(bucketSEXP);
    Rcpp::traits::input_parameter< string >::type file(fileSEXP);
    Rcpp::traits::input_parameter< bool >::type is_read(is_readSEXP);
    Rcpp::traits::input_parameter< bool >::type is_text(is_textSEXP);
    Rcpp::traits::input_parameter< bool >::type UTF8(UTF8SEXP);
    Rcpp::traits::input_parameter< bool >::type auto_open(auto_openSEXP);
    Rcpp::traits::input_parameter< double >::type buff_length(buff_lengthSEXP);
    Rcpp::traits::input_parameter< string >::type description(descriptionSEXP);
    Rcpp::traits::input_parameter< string >::type open_mode(open_modeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type billing_project(billing_projectSEXP);
    rcpp_result_gen = Rcpp::wrap(get_bucket_connection(bucket, file, is_read, is_text, UTF8, auto_open, buff_length, description, open_mode, billing_project));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_GCSConnection_C_package_onLoad", (DL_FUNC) &_GCSConnection_C_package_onLoad, 1},
    {"_GCSConnection_get_bucket_connection", (DL_FUNC) &_GCSConnection_get_bucket_connection, 10},
    {NULL, NULL, 0}
};

void init_altrep(DllInfo* dll);
RcppExport void R_init_GCSConnection(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    init_altrep(dll);
}
