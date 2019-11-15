#define CHECK_ERROR(x)                                         \
  if(x<0) Rf_error(getLastError());                            \
  
#define LLong long long int 

#define TOCHAR(x) CHAR(Rf_asChar(x))
#define CONNECTION_CLASS "googleBucket"