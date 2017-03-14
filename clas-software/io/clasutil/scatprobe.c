#include <stdio.h>
#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;
char ** Scat_Probe ();

/* ----------------------------------------------------- */
int ScatProbe (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int n;
  char **wargs;

  if (argc != 2) {
    itcl_f_interp->result = "ScatProbe: usage: host";
    return TCL_ERROR;
  }

  printf ("Scat Process running on %s\n", argv[1]);
  wargs = Scat_Probe (argv[1]);
  n = 0;
  while (wargs[n] != 0) {
    printf ("%s\n", wargs[n]);
    n++ ;
  }
  return TCL_OK;
}
