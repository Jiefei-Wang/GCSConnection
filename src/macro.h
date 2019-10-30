#define CHECK_ERROR(x)                                         \
  if(x<0) Rf_error(getLastError());