      SUBROUTINE FILL_TBTK_NT(ierr)
c
c
c  Author  :    G. Gavalian 
c  Created :    Mon Mar 09, 1998
c  -------
c
c
c
      IMPLICIT NONE
      SAVE
c
c
c
#include "bcs.inc"
#include "tbtk_inf.inc"
c
c
      INTEGER ierr
c
c 
c 
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_tbtk_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/11/01 02:52:18 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_tbtk_nt.F,v 1.8 2001/11/01 02:52:18 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_TBTK_NT')
      PARAMETER (CRAUTH='Gagik Gavalian')
c
c
      INTEGER NWRITE
      INTEGER IDb,nrow,j,Tind
      INTEGER MLINK,Sect
      INTEGER TRK_SC(6)
      INTEGER Lind,Nind,Jc
c  executable code for routine FILL_TBTRK_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS = 'Filling DC tracks N-tuple'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      DO Sect=1,6
         TRK_SC(Sect)=0
      ENDDO

      N_trk=0
      DO Sect=1,6
         IDb=MLINK(iw,'TBLA',Sect)
         IF(IDb.EQ.0) GOTO 992
         
         Nrow=IW(IDb-4)
         TRK_SC(Sect)=Nrow/34
         Jc=0
         DO J=1,nrow
            Nind=IW(IDb+Jc+1)
            Lind=MOD(Nind,100)
            if (Lind.LT.8) then
               Lind = Lind -3   !DC-Layer
            elseif (Lind.GT.9) then
               Lind = Lind -5   !DC-Layer mod. missing Layer 5&6
            else
               Lind = 0
            endif
            Tind=Nind/100
            if (Tind.GT.0 .AND. Tind.LE.20 .AND.
     &           Lind.GT.0 .AND. Lind.LE.34) then
               sect_trk(Tind)   = sect
               tX(Lind,Tind)    = RW(IDb+Jc+2)
               tY(Lind,Tind)    = RW(IDb+Jc+3)
               tZ(Lind,Tind)    = RW(IDb+Jc+4)
               tCX(Lind,Tind)   = RW(IDb+Jc+5)
               tCY(Lind,Tind)   = RW(IDb+Jc+6)
               tCZ(Lind,Tind)   = RW(IDb+Jc+7)
               TLEN(Lind,Tind) = RW(IDb+Jc+8)
c------------------------------------------
               wire(Lind,Tind) = IW(IDb+Jc+11)
c------------------------------------------
               Dtime(Lind,Tind)= RW(IDb+Jc+12)
               Alpha(Lind,Tind)= RW(IDb+Jc+13)
               Wlen(Lind,Tind) = RW(IDb+Jc+14)
               fdoca(Lind,Tind) = RW(IDb+Jc+16)
               cdoca(Lind,Tind) = RW(IDb+Jc+17)
            endif
            Jc=Jc+17
         ENDDO
         N_trk=N_trk+TRK_SC(Sect)
 992     CONTINUE
      ENDDO

      RETURN
      END
c
c------------------------------------------------------------------------------




