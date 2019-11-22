#include "Rcpp.h"

extern SEXP package_environment;

//Call a function with arguments
SEXP make_call(const char* func_name);
SEXP make_call(const char* func_name, SEXP x1);
SEXP make_call(const char* func_name, SEXP x1, SEXP x2);
SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3);
SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3, SEXP x4);
SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5);