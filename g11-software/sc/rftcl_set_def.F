      SUBROUTINE RFTCL_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: rftcl_set_def.F,v 1.1 1998/04/17 21:03:14 anciant Exp $
c
c  Documentation for subroutine RFTCL_SET_DEF
c
c  Purpose:  Routine to set default values for RF tcl runtime variables
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
c  Author:   Eric Anciant, april 8th, 1998
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
     1'$Id: rftcl_set_def.F,v 1.1 1998/04/17 21:03:14 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RFTCL_SET_DEF')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE

c_end_var
c
c  executable code for routine RFTCL_SET_DEF:
c----6----------------------------------------------------------------72
c
        CRMESS=' '

c
c  -- by default calibration for the RF are read in the Map Manager
c

      rf_get_cal = 0

      RETURN
      END
c
c------------------------------------------------------------------------------




