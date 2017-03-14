      SUBROUTINE MFM_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_tcl_init.F,v 1.1 1999/02/16 16:50:44 kossov Exp $
c
c  Documentation for subroutine MFM_TCL_INIT
c
c  Purpose:  MFM routine for initializing runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines: TCL_LINKVAR
c  ---------------
c
c  Notes: Just general form of the *_tcl_init.F routine
c  ------
c
c  Author:  Mikhail Kossov     Created:  Mon May  9 17:04:06 EST 1997
C  -------
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
#include "mfm_control.inc"
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
      DATA CRCSID/'$Id: mfm_tcl_init.F,v 1.1 1999/02/16 16:50:44 kossov Exp $'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_tcl_init')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ITCL_INT, ITCL_DOUBLE, ITCL_STRING, ITCL_INT_READ
      DATA ITCL_INT, ITCL_DOUBLE, ITCL_STRING, ITCL_INT_READ/1,2,4,129/
c_end_var
c
c  executable code for routine MFM_TCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        WRITE(CRMESS,*)'Tcl definitions for the MFM package'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c  The below call LINKS the fortran variable to the tcl variable, to 
c  interactive change the value of `my_variable' either:
c     A) type  set my_variable 10  at the prompt or in your INITializing script
c     B) setup the communication shell and : SendMsg "set my_variable 10"
c
      CALL TCL_LINKVAR('MFM_MAG_type',MFM_MAG_type,itcl_int)
      CALL TCL_LINKVAR('MFM_comp_fld',MFM_comp_fld,itcl_int)
      CALL TCL_LINKVAR('MFM_BigTorus',MFM_BigTorus,itcl_double)
      CALL TCL_LINKVAR('MFM_MinTorus',MFM_MinTorus,itcl_double)
      CALL TCL_LINKVAR('MFM_PolTargt',MFM_PolTargt,itcl_double)
C     ---- =========== name in list , value , 1=int,2=double,4=string,129=READ
c
      RETURN
      END
c
c------------------------------------------------------------------------------




