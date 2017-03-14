      SUBROUTINE FILL_TRKS
c
c_begin_doc
c  RCS ID string
c  $Id: fill_trks.F,v 1.12 2006/06/16 15:14:58 stepanya Exp $
c
c  Documentation for subroutine FILL_TRKS
c
c  Purpose:
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters:  Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  Thu Mar 27 22:52:13 EST 1997
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
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "hbt_planes.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_trks.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.12 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/16 15:14:58 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_trks.F,v 1.12 2006/06/16 15:14:58 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_TRKS')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER DCsect, DCtrk, DCindx
      INTEGER IND, INDX, ND, ncol
      INTEGER np_cc, np_sc, np_ec
      DATA ncol/10/
      INTEGER I, ipr, Nentry
      INTEGER MBANK
      EXTERNAL MBANK
c_end_var
c
c  executable code for routine FILL_TRKS:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Writing trigger time to TRKS bank for each matched trek.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
C- Do the loop over the sectors and if there are matched tracks/hits crate TRKS.
      DO I = 1, MAXsector
      Nentry = 0
        Do ipr=1,GPARTICLES
          DCindx = PARTstdc(ipr)
          If(DCindx.Gt.0) THEN
            DCsect = DCsector(DCindx)
            if(DCsect.eq.I)Nentry = Nentry + 1
          EndIf
        EndDo
        IF(Nentry.gt.0)THEN
           IND = MBANK(IW,'TRKS',I,ncol,Nentry)
           if(IND.eq.0)then
              write (CRMESS,'(A)')' Error Creating the bank TRKS'
              call RecMes(CRNAME,'W',CRMESS)
              goto 999
           endif
           ND=0
           Do ipr=1,GPARTICLES
              DCindx = PARTstdc(ipr)
              If(DCindx.Gt.0) THEN
                 DCsect = DCsector(DCindx)
                 DCtrk  = DCitrk(DCindx)
                 np_cc=partstcc(ipr)
                 np_sc=partstsc(ipr)
                 np_ec=partstec(ipr)
                 if(DCsect.eq.I)then
                    INDX=IND+ND*ncol
                    IW(INDX+1)=DCtrk+200
                    RW(INDX+2)=PARTbetta(ipr)
                    RW(INDX+3)=Ttime !to be modified for photon beam
                    if(np_cc.GT.0) then
                       RW(INDX+4)=cctime(np_cc)
                       IW(INDX+8)=ccihit(np_cc)
                    endif
                    if(np_sc.GT.0) then
                       RW(INDX+5)=sctime(np_sc)
                       IW(INDX+7)=scpdid(np_sc) !to be modified for photon beam
                       IW(INDX+9)=scihit(np_sc)
                    endif
                    if(np_ec.GT.0) then
                       RW(INDX+6)=ectime(np_ec)
                       IW(INDX+10)=echit(whole,np_ec)*10000+
     &                      echit(inner,np_ec)*100+echit(outer,np_ec)
                    endif
                    ND=ND+1
                 endif
              EndIf
           EndDo
        ENDIF
      ENDDO
 999  CONTINUE
      RETURN
      END
c
c------------------------------------------------------------------------------




