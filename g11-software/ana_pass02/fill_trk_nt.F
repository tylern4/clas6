      SUBROUTINE FILL_TRK_NT(ierr)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_trk_nt.F,v 1.2 1997/12/29 19:15:12 stepanya Exp $
c
c  Documentation for subroutine FILL_TRK_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1 if there was(were) 
c                     treck(s) in DC
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
c  Author:    S. Stepanyan     Created:  Sat Feb 15 18:10:08 EST 1997
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
#include "sdaswim.inc"
#include "anapatt.inc"
#include "trk_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_trk_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/12/29 19:15:12 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_trk_nt.F,v 1.2 1997/12/29 19:15:12 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_TRK_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      integer ok_hbt_fit(10)
      common/pidst/ok_hbt_fit
      INTEGER NWRITE
      INTEGER loop_trk, scpln
      REAL rscmin
      real pi
      DATA pi/3.141592654/
c_end_var
c
c  executable code for routine FILL_TRK_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine fills N-tuple for DC tracks'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      ntrk=0
      do loop_trk = 1, ntr_link
        IF(ntrk.lt.10)THEN
          ntrk = ntrk + 1
          itrk(ntrk)=ntrk
          isect(ntrk) =itr_sect(loop_trk) 
*
          ptrk(ntrk) = trkp(7,1,loop_trk)
          chtrk(ntrk) = trkp(9,1,loop_trk)
          xtrktrg(ntrk) = 
     +-trkp(3,1,loop_trk)*sin((itr_sect(loop_trk)-1.)*pi/3.)+
     +trkp(2,1,loop_trk)*cos((itr_sect(loop_trk)-1.)*pi/3.)
          ytrktrg(ntrk) = 
     +trkp(3,1,loop_trk)*cos((itr_sect(loop_trk)-1.)*pi/3.)+
     +trkp(2,1,loop_trk)*sin((itr_sect(loop_trk)-1.)*pi/3.)
          ztrktrg(ntrk) = trkp(1,1,loop_trk)
          cxtrktrg(ntrk) = 
     +trkp(5,1,loop_trk)*cos((itr_sect(loop_trk)-1.)*pi/3.)
     +-trkp(6,1,loop_trk)*sin((itr_sect(loop_trk)-1.)*pi/3.)
          cytrktrg(ntrk) = 
     +trkp(6,1,loop_trk)*cos((itr_sect(loop_trk)-1.)*pi/3.)+
     +trkp(5,1,loop_trk)*sin((itr_sect(loop_trk)-1.)*pi/3.)
          cztrktrg(ntrk) = trkp(4,1,loop_trk)
*
*
        ENDIF
      enddo
      if ( ntrk.gt.0 ) ierr=1
      RETURN
      END
c
c------------------------------------------------------------------------------




