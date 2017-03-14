      SUBROUTINE ST_BEVT
c
c_begin_doc
c  RCS ID string
c  $Id: st_bevt.F,v 1.3 1997/10/13 13:50:41 staylor Exp $
c
c  Documentation for subroutine ST_BEVT
c
c  Purpose:  ST supplied Beggin EVENT processing routine, called once per event
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: st_bevt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/10/13 13:50:41 $')
      PARAMETER ( CAUTHO = '$Author: staylor $')
      DATA CRCSID/
     1'$Id: st_bevt.F,v 1.3 1997/10/13 13:50:41 staylor Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='ST_BEVT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local St defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
c_end_var
c
c  executable code for routine ST_BEVT:
c  -------------------------------------
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='STart counter begin event routine called.'
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF
c
c
       call St_reset_arrays
c       
       RETURN
       END
c
c----------------------------------------------------------------------




