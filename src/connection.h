#ifdef __cplusplus
#include <Rcpp.h>
extern "C" {
#else
#include "Rinternals.h"
#endif
	void* createBuckekConnectionCPP(SEXP credentials, SEXP project, SEXP bucket, SEXP file);
	void openBucketConnectionCPP(void* cbc);
	void closeBucketConnectionCPP(void* cbc);
	void destropBucketConnectionCPP(void* cbc);
	size_t readBucketConnectionCPP(void* target, size_t sz, size_t ni, void* cbc);
	size_t writeBucketConnectionCPP(const void* target, size_t sz, size_t ni, void* cbc);
#ifdef __cplusplus
}
#endif