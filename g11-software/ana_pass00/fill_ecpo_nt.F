      SUBROUTINE FILL_ECPO_NT
c
c_begin_doc
c  RCS ID string
c  $Id: fill_ecpo_nt.F,v 1.2 1999/03/09 17:40:38 boiarino Exp $
c
c  Documentation for subroutine FILL_ECPO_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1  
c                     
c
c  Output Parameters:  Fill's areas in ECPO_NT common block.
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Aug Dec 14 00:42:15 EST 1998
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
#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "ec_strip.inc"
#include "ec_peak.inc"
#include "ec_control.inc"
#include "EcStrip.CMN"
#include "bcs.inc"
#include "ecpo_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_ecpo_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:38 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: fill_ecpo_nt.F,v 1.2 1999/03/09 17:40:38 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_ECPO_NT')
      PARAMETER (CRAUTH='stepanya')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER  loop
      INTEGER  ind,indx
      INTEGER  MAMIND
      INTEGER IECPO
      DATA IECPO/0/
c_end_var
c
c  executable code for routine FILL_ECPO_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Ntuple from ECPO'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(IECPO.eq.0)IECPO=MAMIND(IW,'ECPO')
      IF(IECPO.GT.0)ind=IW(IECPO)
      IF(IND.GT.0)THEN
        nhit=IW(ind-4)
        indx=ind
        do loop=1,nhit
          slh(loop)=iw(indx+7)
          uid1(loop)=iw(indx+1)
          uid2(loop)=iw(indx+2)
          vid1(loop)=iw(indx+3)
          vid2(loop)=iw(indx+4)
          wid1(loop)=iw(indx+5)
          wid2(loop)=iw(indx+6)
          indx=indx+7
        enddo
      ENDIF
99    RETURN
      END
c
c------------------------------------------------------------------------------




