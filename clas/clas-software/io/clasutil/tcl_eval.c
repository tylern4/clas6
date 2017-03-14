/*
 *  Documentation for subroutine TCL_EVAL_
 *
 *  Purpose:  provides a wrapper for Tcl_Eval found in libtcl, Tcl_Eval
 *  --------  evaluates a Tcl SCRIPT, from fortran the use is as follows:
 *            call tcl_eval('set fpk_input myfile.bfp')
 *
 *  Notes: see page 290 in Ousterhout's book
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
/*  executable code for routine TCL_EVAL_:
 *  ------------------------------------- */
int tcl_eval_(cval, ilen)
char *cval;
int ilen;
{
  char *newcval;
  extern Tcl_Interp *itcl_f_interp;
  int code;

  newcval=(char *)malloc(ilen+1);
  strncpy(newcval,cval,ilen);
  newcval[ilen]=0;

/*  printf ("---------<%s>--------\n", newcval);  */
  code = Tcl_Eval(itcl_f_interp, newcval);
  if (code != 0) {
    /* printf ("\t%s\n", itcl_f_interp->result);  */
    return TCL_OK;
  }
  else {
    /* printf ("\t%s\n", itcl_f_interp->result);   */
    return TCL_ERROR;
  }
}
/*-----------------------------------------------------------------------*/



