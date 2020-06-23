#include <stdlib.h>
#include "calib_manager_tcl.h"

void calib_tcl_init() {
  char *ptr;
  static int init = 0;

  if (init) return;
  init = 1;

  f_init_tcl_(); /* create interpreter */
  
  RunIndex_table = (char *) malloc(32);
  
  if(ptr=getenv("CLAS_CALDB_RUNINDEX")){
    strcpy(RunIndex_table,ptr);
  } else{
    strcpy(RunIndex_table, "RunIndex");
  } 
 
  Tcl_LinkVar(itcl_f_interp, "RunIndex_table", &RunIndex_table, TCL_LINK_STRING);

}

void calib_tcl_init_() {
  calib_tcl_init();
}
