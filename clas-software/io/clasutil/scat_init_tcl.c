/*
 *  $Id: scat_init_tcl.c,v 1.1.1.1 1997/03/28 20:49:56 marki Exp $
 *
 *  Documentation for subroutine SCAT_INIT_TCL.C
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
      static char cfile[] =  "$RCSfile: scat_init_tcl.c,v $";
      static char crevis[] = "$Revision: 1.1.1.1 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1997/03/28 20:49:56 $";
      static char cautho[] = "$Author: marki $";
/*   Module information: */
      static char crname[] = "scat_init_tcl.c";
      static char crauth[] = "James Muguira";
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine SCAT_INIT_TCL.C:
 *  -------------------------------------
*/
extern Tcl_Interp *itcl_f_interp;
extern int ScatProbe ();
extern int ScatLocate ();
extern int ScatKill ();
extern int ScatDebug () ;
extern int StartPgm ();
extern int ScatInitConnection ();
extern void dump_handles ();

void scat_init_tcl_()
{
   Tcl_CreateCommand (itcl_f_interp, "startprogram", StartPgm, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatProbe", ScatProbe, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatLocate", ScatLocate, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatKill", ScatKill, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatDebug", ScatDebug, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatInitConnection", ScatInitConnection, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);

   Tcl_CreateCommand (itcl_f_interp, "ScatDumpHandles", dump_handles, (ClientData)NULL, 
		      (Tcl_CmdDeleteProc *)NULL);


 return;
}
/*-----------------------------------------------------------------------*/



