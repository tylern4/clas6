/*
 *  Documentation for subroutine TCL_RECSIS_EVALFILE_
 *
 *  Purpose:  provides a wrapper for Tcl_Evalfile found in libtcl
 *  --------
 *
 *  Notes:
 *  ------
 *
 *  Author:   Arne Freyberger      Created:  Wed Jan 24 15:45:37 EST 1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *_end_doc*/
/*_begin_inc*  include files */
/*  --------------------- */
#include <tcl.h>
/*_end_inc */
/*  executable code for routine TCL_RECSIS_EVALFILE_:
 *  ------------------------------------- */
int tcl_recsis_evalfile_(cval, ilen)
char *cval;
int ilen;
{
  char *newcval;
  extern Tcl_Interp *itcl_f_interp;
  int code;

  newcval=(char *)malloc(ilen+1);
  strncpy(newcval,cval,ilen);
  newcval[ilen]=0;

  code = Tcl_EvalFile(itcl_f_interp, newcval);
  if (code != 0) {
    printf ("\t%s\n", itcl_f_interp->result);
    return 0;
  }
  else {
    printf ("\t%s\n", itcl_f_interp->result);
    return -1;
  }
}
/*-----------------------------------------------------------------------*/



