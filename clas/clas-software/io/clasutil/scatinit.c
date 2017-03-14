/*
 *  $Id: scatinit.c,v 1.1.1.1 1997/03/28 20:50:03 marki Exp $
 *  Documentation for subroutine scatinit.C
 *
 *  Purpose:
 *  --------
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------
 *
 *  Output Parameters:  (Name - Type - Meaning)
 *  -----------------
 *
 *  Called from:
 *  ------------
 *
 *  Other routines:
 *  ---------------
 *
 *  Notes:
 *  ------
 *
 *  Author:   James Muguira      Created:  monday may 6, 1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *
 *
 *_end_doc
/*_begin_var
 *  input/output variables:
 *  -----------------------
 *
 *  Local pre-defined variables:
 *  ---------------------------
 *  RCS information:
*/
      static char cfile[] =  "$RCSfile: scatinit.c,v $";
      static char crevis[] = "$Revision: 1.1.1.1 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1997/03/28 20:50:03 $";
      static char cautho[] = "$Author: marki $";
/*   Module information: */
      static char crname[] = "startpgm.c";
      static char crauth[] = "James Muguira";
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine startpgm.C:
 *  -------------------------------------
*/
#include <stdio.h>
#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;

/* ----------------------------------------------------- */
int ScatInitConnection (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int code, h;
  unsigned long p, v;
  char bb[2048];

  if (argc != 4) {
    itcl_f_interp->result = "ScatInitConn: usage: host pgm_no pgm_vers";
    return TCL_ERROR;
  }
  p = atoi (argv[2]);
  v = atoi (argv[3]);

  h = Scat_InitHandle (argv[1], p, v);
  sprintf (bb, "set scat_handle %d ", h);

  printf ("i con: %s %d %d : %s\n", argv[1], p, v, bb);

  code = Tcl_Eval (itcl_f_interp, bb);
  if (code != TCL_OK)
    return TCL_ERROR;
  
  return TCL_OK;
}
