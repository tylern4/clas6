      SUBROUTINE FILL_CCRC_NT (ierr)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_ccrc_nt.F,v 1.2 1997/06/24 04:05:26 stepanya Exp $
c
c  Documentation for subroutine FILL_CCRC_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1 if there was(were) 
c                     hit(s) in CC
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
c  Author:   S. Stepanyan      Created:  Sun Feb 16 13:26:24 EST 1997
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
#include "ccrc_nt.inc"
#include "cc_rc.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_ccrc_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/06/24 04:05:26 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_ccrc_nt.F,v 1.2 1997/06/24 04:05:26 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_CCRC_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER loop_cchit
c_end_var
c
c  executable code for routine FILL_CCRC_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine fills CC N-tuple'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      ncchit=0
      IF ( nrclust.ne.0 ) THEN
        DO loop_cchit=1,nrclust
          icchit(loop_cchit)=loop_cchit
          ccsect(loop_cchit) = nrsect(loop_cchit)
          ccsegm(loop_cchit) = nrsegm(loop_cchit)
          ccphe(loop_cchit) = nrphe(loop_cchit)
          cctime(loop_cchit) = nrtime(loop_cchit)*0.05
          cctheta(loop_cchit) = nrthet(loop_cchit)
          ccphi(loop_cchit) = nrphy(loop_cchit)
        ENDDO
        ncchit=nrclust
        if(ncchit.gt.0)ierr=0
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




