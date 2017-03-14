#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>

void read_ddl(){
  char *par = "CLAS_PARMS";
  char filename[1024];
  char *ddl = "clasbanks.ddl";

  sprintf(filename, "%s/%s", getenv(par), ddl);
  txt2bos_(filename, strlen(filename));
}
