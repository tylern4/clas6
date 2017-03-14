/*
 *  $Id: startpgm.c,v 1.1.1.1 1997/03/28 20:49:57 marki Exp $
 *  Documentation for subroutine startpgm.C
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
      static char cfile[] =  "$RCSfile: startpgm.c,v $";
      static char crevis[] = "$Revision: 1.1.1.1 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1997/03/28 20:49:57 $";
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
int StartPgm (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int code ;
  char bb[2048];

  if (argc != 5) {
    itcl_f_interp->result = "start program: usage: startprogram host pgmName loginId id";
    return TCL_ERROR;
  }
  code = atoi (argv[3]);

  (void)Scat_StartProgram (argv[1], argv[2], argv[4], code);
  sleep (3);
  return TCL_OK;
}
