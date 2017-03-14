/*
 *  $Id: tcl_linkchar.c,v 1.1.1.1 1997/03/28 20:49:58 marki Exp $
 *
 *  Documentation for subroutine TCL_LINKCHAR.C
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
 *  Author:   James Muguira      Created:  Wed Feb 14 14:55:47 EST 1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *
 *
 *_end_doc
 */
#include <stdio.h>
#include <tcl.h>
/*_begin_var
 *  input/output variables:
 *  -----------------------
 *
 *  Local pre-defined variables:
 *  ---------------------------
 *  RCS information:
*/
      static char cfile[] =  "$RCSfile: tcl_linkchar.c,v $";
      static char crevis[] = "$Revision: 1.1.1.1 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1997/03/28 20:49:58 $";
      static char cautho[] = "$Author: marki $";
/*   Module information: */
      static char crname[] = "TCL_LINKCHAR.C";
      static char crauth[] = "James Muguira";
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine TCL_LINKCHAR.C:
 *  -------------------------------------
*/
int tcl_linkchar_ (cs, v, i, varLen, fort_supplied_len)
char   *cs;
char   *v;
int    *i;
int    *varLen;
int    fort_supplied_len;
{
  extern Tcl_Interp *itcl_f_interp;
  int code;
  char *tbuf, *cval;

  cval = (char *)malloc (fort_supplied_len+1);
  strncpy (cval, cs, fort_supplied_len);
  cval[fort_supplied_len] = 0;

/*
 * make a tcl command to put the name and length of the 
 * fortran string into the tcl interp.
 */
  tbuf = (char *)malloc (1024);
  sprintf (tbuf, "set rchar_control(%s,1) %d ; \
           set rchar_control(%s,2) %d", cval, v, cval, *varLen);

  code = Tcl_Eval (itcl_f_interp, tbuf);
  if (code != TCL_OK)
    return 0;  /* return a fortran false ! */

  free (tbuf);
  return 1;   /* return a fortran true */
}
