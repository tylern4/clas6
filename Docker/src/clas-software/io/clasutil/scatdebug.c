#include <stdio.h>
#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;
extern int scat_debuglevel;

/* ----------------------------------------------------- */
int ScatDebug (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int n;

  if (argc != 2) {
    itcl_f_interp->result = "ScatDebug: usage ScatDebug 0/1" ; 
    return TCL_ERROR;
  }

  scat_debuglevel = atoi (argv[1]) ;
  printf ("scat debugging level = %d\n", scat_debuglevel);

  return TCL_OK;
}
