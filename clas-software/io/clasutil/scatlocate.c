#include <stdio.h>
#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;
int Scat_LocateProgram ();

/* ----------------------------------------------------- */
int ScatLocate (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int n;

  if (argc != 3) {
    itcl_f_interp->result = "ScatLocate: usage: host pgmName";
    return TCL_ERROR;
  }

  printf ("Looking for: <%s> Scat Process running on %s\n", argv[2], argv[1]);
  n = Scat_LocateProgram (argv[1], argv[2]);
  if (n < 0)
    printf ("Sorry did not find: %s on %s\n", argv[2], argv[1]);
  else
    printf ("Ok I found: %s running on %s: handle = %d\n", argv[2], argv[1], n); 


  return TCL_OK;
}
