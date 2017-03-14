/*
 *  $Id: tcl_linkvar.c,v 1.1.1.1 1997/03/28 20:49:58 marki Exp $
 *
 *  Documentation for subroutine TCL_LINKVAR.C
 *
 *  Purpose:
 *  --------
 *  create a connection between fortran and tcl variables. This 
 *  routine will ONLY do integers, double and logical.  Look in
 *  tcl_linkchar for character strings.
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  
 *  ----------------
 *
 *  Output Parameters:  
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
 *  Author:   James Muguira      Created:  Wed Feb 14 15:10:06 EST 1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *
 *
 *_end_doc
 */
#include <tcl.h>
/*_begin_var
 *  input/output variables:
 *  -----------------------
 *
 *  Local pre-defined variables:
 *  ---------------------------
 *  RCS information:
*/
static char cfile[] =  "$RCSfile: tcl_linkvar.c,v $";
static char crevis[] = "$Revision: 1.1.1.1 $";
static char cstate[] = "$State: Exp $";
static char cdate[] =  "$Date: 1997/03/28 20:49:58 $";
static char cautho[] = "$Author: marki $";

static char crname[] = "tcl_linkvar.c";
static char crauth[] = "James Muguira";

extern Tcl_Interp *itcl_f_interp;

/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine TCL_LINKVAR.C:
 *  -------------------------------------
*/
void tcl_linkvar_(cval, value, ibit, ilen)
char *cval;
int *value;
int *ibit;
int ilen;
{
  char *newcval, *tbuf;

  int code;
  
  newcval=(char *)malloc(ilen+1);
  strncpy(newcval,cval,ilen);
  newcval[ilen]=0;

  tbuf = (char *)malloc (1024 );
  sprintf (tbuf, "set run_control(%s) %s ;", newcval, newcval);
  
  code = Tcl_LinkVar(itcl_f_interp, newcval, (char *) value, *ibit);
  if (code == TCL_OK)
    Tcl_Eval (itcl_f_interp, tbuf);
  free (tbuf);
  return;
}
/*-----------------------------------------------------------------------*/



