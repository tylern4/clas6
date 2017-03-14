      SUBROUTINE JANR_EVGen
c
c_begin_doc
c  RCS ID string
c  $Id: janr_evgen.F,v 1.1 2003/11/20 05:16:08 lcsmith Exp $
c
c  Documentation for subroutine JANR_MAIN
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Dec 11 15:40:59 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc`
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
#include "janrtab.inc"
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: janr_evgen.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2003/11/20 05:16:08 $')
      PARAMETER (CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/   
     1'$Id: janr_evgen.F,v 1.1 2003/11/20 05:16:08 lcsmith Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='JANR_EvGen')
      PARAMETER (CRAUTH='S.Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ierr
c_end_var
c
      CALL janr_tab(ierr)
      IF ( ierr.EQ. 0 ) THEN
         If ( MaxEvents.Gt.0 ) Then
         EndIf
      ENDIF
c  executable code for routine JANR_EvGen:
c----6----------------------------------------------------------------72
      END





