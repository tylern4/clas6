      SUBROUTINE RFTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: rftcl_init.F,v 1.1 1998/04/17 21:03:11 anciant Exp $
c
c  Documentation for subroutine RFTCL_INIT
c
c  Purpose:  routine for initializing RF tcl runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric Anciant, april 8th 1998
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
c
#include "rftcl.CMN"
c
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: rftcl_init.F,v 1.1 1998/04/17 21:03:11 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RFTCL_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE
      INTEGER ITCL_INT, ITCL_STRING, ITCL_INT_READ
      DATA ITCL_INT, ITCL_STRING, ITCL_INT_READ/1,4,129/
c_end_var
c
c  executable code for routine RFTCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS=' rf tcl init routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      endif

      call rftcl_set_def

c
c  tcl variable to define where we get the calibration constants for the RF.
c

      call tcl_linkvar('rf_get_cal',rf_get_cal,itcl_int)


      RETURN
      END
c
c------------------------------------------------------------------------------




