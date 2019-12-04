#include <Rcpp.h>
#include "altrep.h"
#include "macro.h"
#include "R_ext/Altrep.h"

using namespace Rcpp;

R_altrep_class_t altrep_raw_class;

SEXP make_alt_raw(R_xlen_t len, void* ptr) {
	SEXP data = Rf_protect(R_MakeExternalPtr(ptr,R_NilValue,R_NilValue));
	SEXP result = Rf_protect(R_new_altrep(altrep_raw_class, wrap(len), data));
	Rf_unprotect(2);
	return result;
}

R_xlen_t altrep_length(SEXP x) {
	return as<R_xlen_t>(R_altrep_data1(x));
}

void* altrep_dataptr(SEXP x, Rboolean writeable) {
	return R_ExternalPtrAddr(R_altrep_data2(x));
}
const void* altrep_dataptr_or_null(SEXP x)
{
	return R_ExternalPtrAddr(R_altrep_data2(x));
}

//[[Rcpp::init]]
void init_altrep(DllInfo* dll) {
	const char* class_name = "alt_raw";
	altrep_raw_class = R_make_altraw_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(altrep_raw_class, altrep_length);
	R_set_altvec_Dataptr_method(altrep_raw_class, altrep_dataptr);
	R_set_altvec_Dataptr_or_null_method(altrep_raw_class, altrep_dataptr_or_null);
}