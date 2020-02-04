#include "utils.h"
SEXP package_environment;

//package_environment


SEXP make_call(const char* func_name) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang1(func));
	SEXP result = Rf_protect(R_forceAndCall(call, 0, package_environment));
	UNPROTECT(2);
	return result;
}

SEXP make_call(const char* func_name, SEXP x1) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang2(func, x1));
	SEXP result = Rf_protect(R_forceAndCall(call, 1, package_environment));
	UNPROTECT(2);
	return result;
}

SEXP make_call(const char* func_name, SEXP x1, SEXP x2) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang3(func, x1, x2));
	SEXP result = Rf_protect(R_forceAndCall(call, 2, package_environment));
	UNPROTECT(2);
	return result;
}

SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang4(func, x1, x2, x3));
	SEXP result = Rf_protect(R_forceAndCall(call, 3, package_environment));
	UNPROTECT(2);
	return result;
}

SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3, SEXP x4) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang5(func,x1,x2,x3,x4));
	SEXP result = Rf_protect(R_forceAndCall(call, 4, package_environment));
	UNPROTECT(2);
	return result;
}

SEXP make_call(const char* func_name, SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5) {
	SEXP func = Rf_findFun(Rf_install(func_name), package_environment);
	SEXP call = Rf_protect(Rf_lang6(func, x1, x2, x3, x4, x5));
	SEXP result = Rf_protect(R_forceAndCall(call, 5, package_environment));
	UNPROTECT(2);
	return result;
}


