/*
 *  $Id: f_init_tcl.c,v 1.2 2001/02/06 14:47:56 marki Exp $
 *
 *  Documentation for subroutine F_INIT_TCL.C
 *
 *  Purpose:
 *  --------
 *    create tcl interpreter in a fortran program.
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters: NONE
 *  ----------------
 *
 *  Output Parameters:  NONE
 *  -----------------
 *
 *  Called from: 
 *  ------------
 *    fortran main routine
 *
 *  Other routines:
 *  ---------------
 *
 *  Notes:
 *  ------
 *
 *  Author:   James Muguira      Created:  Wed Feb 14 14:45:29 EST 1996
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
      static char cfile[] =  "$RCSfile: f_init_tcl.c,v $";
      static char crevis[] = "$Revision: 1.2 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 2001/02/06 14:47:56 $";
      static char cautho[] = "$Author: marki $";
/*   Module information: */
      static char crname[] = "f_init_tcl.c";
      static char crauth[] = "James Muguira";
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine F_INIT_TCL.C:
 *  -------------------------------------
*/
Tcl_Interp *itcl_f_interp;
extern int SetChar ();

void f_init_tcl_()
{
   static int init = 0;
  
   if (init) return; /* only define the interpreter once */
   init = 1;

   itcl_f_interp = Tcl_CreateInterp();

   Tcl_CreateCommand (itcl_f_interp, "setc", SetChar, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

 return;
}
/*-----------------------------------------------------------------------*/



