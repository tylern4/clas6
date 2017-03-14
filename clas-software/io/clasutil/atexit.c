/*
 *  $Id: atexit.c,v 1.1.1.1 1997/03/28 20:49:54 marki Exp $
 *
 *  Documentation for subroutine ATEXIT.C
 *
 *  Purpose:
 *  --------
 *  c wrapper so FORTRAN can register subroutines with the exit handle
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  ROUTINE : CHAR : routine to registered
 *  ----------------
 *
 *  Author:  Arne Freyberger 6-june-1996
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *
 *
 *_end_doc
 */
/*
 *  Local User defined variables:
 *  -----------------------------
 *
 *_end_var
 *  executable code for routine ATEXIT.C:
 *  -------------------------------------
*/
int *atexit_ (fcn)
void (*fcn)();
{
  int status;
  status = atexit(fcn);
  return (int*) status;
}
/*-----------------------------------------------------------------------*/



