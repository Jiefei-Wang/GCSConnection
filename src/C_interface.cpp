#include <Rcpp.h>
#include <vector>
#include <string>
#include "macro.h"
#include "utils.h"
#include "connection.h"


// [[Rcpp::export]]
void C_package_onLoad(SEXP pkg_namespace) {
	package_environment = pkg_namespace;
}



