#include <stdio.h>
#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;
int Scat_SendMsgNr ();

/* ----------------------------------------------------- */
int ScatKill (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int i, z;

  if (argc != 2) {
    itcl_f_interp->result = "ScatKill: usage: handle";
    return TCL_ERROR;
  }

  z = atoi (argv[1]);
  
  i = Scat_SendMsgNr (z, "die now");;

  return TCL_OK;
}
