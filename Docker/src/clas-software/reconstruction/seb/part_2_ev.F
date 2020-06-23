      SUBROUTINE PART_2_EV(i,j)
c
c_begin_doc
c  RCS ID string
c  $Id: part_2_ev.F,v 1.7 2006/06/15 21:36:40 stepanya Exp $
c
c  Documentation for subroutine PART_2_EV
c
c  Purpose:
c  --------
c
c  Input Parameters:  INTEGER i - row index for particle structure
c  ----------------   INTEGER j - row index for event structure
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines: Moves final reconstructed particles to the event record.
c  --------------- 
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  Thu Apr  3 23:08:09 EST 1997
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
c#include "bcs.inc"
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER i,j
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: part_2_ev.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 21:36:40 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: part_2_ev.F,v 1.7 2006/06/15 21:36:40 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='PART_2_EV')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, LA
c_end_var
c
c  executable code for routine PART_2_EV:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine fills event record'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      EVID(j) = PARTID(i) 
      EVP(j) = PARTP(i)
      EVM(j) = PARTM(i)
      EVQ(j) = PARTQ(i)
      EVBETTA(j) = PARTBETTA(i)
      EVCX(j) = PARTCX(i)
      EVCY(j) = PARTCY(i)
      EVCZ(j) = PARTCZ(i)
      EVVERTX(j) = PARTVERTX(i)
      EVVERTY(j) = PARTVERTY(i) 
      EVVERTZ(j) = PARTVERTZ(i)
      EVCHI2DC(j) = PARTCHI2DC(i)
      EVCHI2CC(j) = PARTCHI2CC(i)
      EVCHI2SC(j) = PARTCHI2SC(i)
      DO la=WHOLE,OUTER
        EVCHI2EC(la,j) = PARTCHI2EC(la,i)
      ENDDO
      EVCHI2LEC(j) = PARTCHI2LEC(i)
      EVSTATUS(j) = PARTSTATUS(i)
      RETURN
      END
c
c------------------------------------------------------------------------------




