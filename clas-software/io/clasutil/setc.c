/*
 *  $Id: setc.c,v 1.4 2007/08/22 01:49:52 holtrop Exp $
 *  Documentation for subroutine SETC.C
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
 *  Author:   James Muguira      Created:  Wed Feb 14 14:50:51 EST 1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *  David Turissin - change to echo the current setting of the variable
 *                   if no additional parms are sent.
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
      static char cfile[] =  "$RCSfile: setc.c,v $";
      static char crevis[] = "$Revision: 1.4 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 2007/08/22 01:49:52 $";
      static char cautho[] = "$Author: holtrop $";
/*   Module information: */
      static char crname[] = "setc.c";
      static char crauth[] = "James Muguira";
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine SETC.C:
 *  -------------------------------------
*/
#include <stdio.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif

#include <tcl.h>

extern Tcl_Interp *itcl_f_interp;
extern char *strdupfort ();

/* ----------------------------------------------------- */
int SetChar (clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp *interp;
int argc;
char *argv[];
{
  int len_tclstr, len_inpt, code, i;
  int pint;
  char *p,*z, *tbuf;

  if (argc < 2) {
    itcl_f_interp->result = "usage: setc <string> <string>\n";
    return TCL_ERROR;
  }

  if (argc == 2) {
    tbuf = (char *)malloc (1024);
    if (tbuf == 0) {
      itcl_f_interp->result = "no memory for setc";
      return TCL_ERROR;
    }
    sprintf (tbuf, "set rchar_control(%s,1)", argv[1]);

    code = Tcl_Eval (itcl_f_interp, tbuf);
    if (code != TCL_OK)
      return TCL_ERROR;

    sscanf (itcl_f_interp->result, "%d", &pint);
    p=(char *)pint;

    sprintf (tbuf, "set rchar_control(%s,2)", argv[1]);

    code = Tcl_Eval (itcl_f_interp, tbuf);
    if (code != TCL_OK)
      return TCL_ERROR;

    sscanf (itcl_f_interp->result, "%d", &len_tclstr);
    (void)free (tbuf);
    tbuf = (char *)strdupfort (p, len_tclstr);
    printf ("<%s>\n", tbuf);
    Tcl_SetResult(itcl_f_interp, tbuf, TCL_DYNAMIC);

    return TCL_OK;
  }
  else {
    /* 
     * must have more than 2 args - I'll change the fortran 
     * string ...
     */
    
    tbuf = (char *)malloc (1024);
    /*
     * ask the tcl interp to return the addr of the 
     * fortran string.
     */
    sprintf (tbuf, "set rchar_control(%s,1)", argv[1]);
    
    code = Tcl_Eval (itcl_f_interp, tbuf);
    if (code != TCL_OK)
      return TCL_ERROR;
    
    /*
     * move the addr to the pointer p
     */
    sscanf (itcl_f_interp->result, "%d", &pint);
    p=(char *)pint;
    /*
     * now get the length from the tcl interp
     */
    sprintf (tbuf, "set rchar_control(%s,2)", argv[1]);
    
    code = Tcl_Eval (itcl_f_interp, tbuf);
    if (code != TCL_OK)
      return TCL_ERROR;
    
    sscanf (itcl_f_interp->result, "%d", &len_tclstr);
    
    len_inpt = strlen (argv[2]);
    
    if (len_tclstr < len_inpt) {
      sprintf (itcl_f_interp->result, "input string too long");
      return TCL_ERROR;
    }
    
    for (i=0, z=argv[2]; i<len_inpt; i++)
      *p++ = *z++;
    
    for (; i<len_tclstr; i++)
      *p++ = ' ';
    
    free (tbuf);
  }
  return TCL_OK;
}
