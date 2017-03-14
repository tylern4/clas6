nocap/*      subroutine template */
nocap/*
nocap *_begin_doc
nocap *  RCS ID string
nocap *  $Id: template.c,v 1.2 1997/12/03 23:54:17 feuerbac Exp $
nocap*/
nocap/*
nocap *  Documentation for subroutine template
nocap *
nocap *  Purpose:
nocap *  --------
nocap *
nocap *  Calling Sequence:
nocap *  ----------------
nocap *
nocap *  Input Parameters:  (Name - Type - Meaning)
nocap *  ----------------
nocap *
nocap *  Output Parameters:  (Name - Type - Meaning)
nocap *  -----------------
nocap *
nocap *  Called from:
nocap *  ------------
nocap *
nocap *  Other routines:
nocap *  ---------------
nocap *
nocap *  Notes:
nocap *  ------
nocap *
nocap *  Author:   John Doe      Created:  day-mon-xx
nocap *  -------
nocap *
nocap *  Major revisions:
nocap *  ----------------
nocap *     
nocap *
nocap *_end_doc
nocap */
nocap/*
nocap *_begin_inc
nocap *  include files :
nocap *  ---------------------
nocap * BOS common block  uncomment the next line for BOS include file */
nocap/*#include <ntypes.h> */
nocap/*#include <bostypes.h> */
nocap/*_end_inc */
nocap/*
nocap *  Local pre-defined variables:
nocap *  ---------------------------  
nocap *  RCS information: 
nocap*/
nocap#define USE(var) static void * use_##var = (void *) &var
nocap/* keep this all on one line so CVS can recognize this */
nocapstatic char crcsid[] =
nocap   "$Id: template.c,v 1.2 1997/12/03 23:54:17 feuerbac Exp $";
nocapUSE(crcsid);  /* make sure it is not optimized away */
nocap
nocap/*   Module information: */
nocap      static char crname[] = "template";
nocap      static char crauth[] = "John Doe";
nocap/*
nocap *  executable code for template:
nocap *  -------------------------------------
nocap*/
nocap void template(){
nocap return;
nocap}
nocap/*
nocap*-------------------------------------------------------------------------
nocap*/



