#define CHECK_ERROR(x)                                         \
  if(x<0) Rf_error(getLastError());                            \
  
#define LLong long long int 

#define TOCHAR(x) CHAR(Rf_asChar(x))
#define CONNECTION_CLASS "googleBucket"


#define PACKAGE_NAME "googleCloudStorageStream"
#define PACKAGE_ENV_NAME "namespace:" PACKAGE_NAME
#define PACKAGE_NAMESPACE R_FindNamespace(Rf_mkString(PACKAGE_NAME))

