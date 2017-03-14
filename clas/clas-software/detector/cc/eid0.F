      SUBROUTINE EID0(Nelec)
c     ======================
c
c_begin_doc
c  RCS ID string
c  $Id: eid0.F,v 1.6 2001/10/30 16:09:00 boiarino Exp $
c
c  Documentation for subroutine EID0
c
c  Purpose:   Fast electron identification based on step0
c  --------   reconstruction for CC,SC,EC (without tracking)
c
c
c  Input Parameters:  NONE (BOS - bank info after step0 reconstruction)
c  ----------------
c
c  Output Parameters:  Nelec - Integer - number of electrons found
c  -----------------   Other parameters - in /CCEID0/ (see cccontr.inc)
c
c
c  Other routines:     clas2sect - to transform ec coordinates to the
c  ---------------     Sector reference system
c
c
c  Author:   Alexander Vlassov    Created:  Thu Oct 31 13:34:59 EST 1996
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
#include "bcs.inc"
#include "wcs.inc"
#include "eidcontr.inc"
#include "cc_rc.inc"
#include "ccechb.inc"
#include "ccschb.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      integer Nelec
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: eid0.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/10/30 16:09:00 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: eid0.F,v 1.6 2001/10/30 16:09:00 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EID0')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, i, ii, j, k, isec, NCOL, ind 
      real ec_dist(6), sc_dist1(6), ec_dwhole
      real rcoord(3), rdir(3), rpln(3), scpos, s
      logical LOCBNK
      character*256 CFMT
      character*8 CDISC(100)
c
      integer MBANK, MLINK
      DATA NWRITE /0/
c_end_var
c
c  executable code for routine EID0:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = 1
        CRMESS='eid0 was called !!!'
        CALL RECMES(CRNAME,'I',CRMESS)
c
        DO isec=1,6
           IND=MLINK(jw,'ECP ',isec)
           IF(IND.NE.0) THEN
c              do i=1,3
c                 n2sect(i,isec) = rjw(IND+i)
c              enddo
              ec_dist(isec) = rjw(IND+4)
              ec_dwhole     = rjw(IND+5)
c          ec_dinner = rjw(IND+6)
c          ec_douter = rjw(IND+7)
           ENDIF
        ENDDO
        DO isec=1,6
           IND=MLINK(jw,'SCP ',isec)
           IF(IND.NE.0)  sc_dist1(isec) = rjw(ind+4)
        ENDDO 
        print *,' ECP ',ec_dist(1),ec_dwhole,'  SCP ',sc_dist1(1)
        print *,' EID0: CCscecy=',CCscecy,' ; CCscecz=',CCscecz,
     &      ' ; CCicoor=',CCicoor,' ; CCscpn=',CCscpn
         LOCBNK = .false.
        call BNKTST('EID0',LOCBNK,NCOL,CFMT,CDISC)
        if(LOCBNK) then
          write(CRMESS,*) 'EID0 bank: Number of columns :',NCOL
          call recmes(CRNAME,'I',CRMESS)

        else

          write(CRMESS,*) ' Can not access to bank EID0 !!!'
          call recmes(CRNAME,'W',CRMESS)
          call mkfmt(IW,'EID0','(4I)')
        end if
c
      ENDIF
c
      Nelec = 0
      if(nrclust.le.0) return
c
      call cc_ech            ! Get EC information
      if(jectrk.le.0) return
      call cc_sch            ! Get SC information
      if(jsctrk.le.0) return
c
      do 100 j = 1,jectrk                               ! Cycle on EC clusters
         if(jeclay(j).ne.9) goto 100                    ! Inner + Outer layers only
         if(ec_e(j).lt. 0.001*CCecthr ) goto 100        ! EC energy deposit
         isec=jecsec(j)
         call clas2sect(isec,ec_r(1,j),rcoord)
         s = sqrt(rcoord(1)**2+rcoord(2)**2+rcoord(3)**2)
         scpos = SC_dist1(isec) - EC_dist(isec) - Ec_dwhole
         do ii=1,3
            rdir(ii) = rcoord(ii)/s
            rpln(ii) = rcoord(ii) + scpos*rdir(ii)
         enddo
c
         do 110 k = 1,jsctrk                            ! Cycle on SC clusters (pairs)
            if( jscsec(k).ne.isec ) goto 110            ! same sector
            if( sc_e(k).lt. 0.0001*CCscthr ) goto 110   ! SC energy deposit
c
            if(abs(rpln(3)-sc_r(3,k)).gt.CCscecz) goto 110  ! Z coor difference
            if(abs(rpln(2)-sc_r(2,k)).gt.CCscecy) goto 110  ! Y coor difference
c
c **  candidate is found in EC & SC  -> search for CC cluster:
c
            if(Nelec .ge. maxeid0) then
c     ==========================> too many electrons !
               write(CRMESS,*) 'Too many electrons  :',Nelec
               CALL RECMES(CRNAME,'W',CRMESS)
               go to 200
            endif

c **  tcl: no matching with CC required:

            if( CCscpn.le.0 ) then
               Nelec = Nelec + 1
               jeidsec(Nelec) = isec
               jCC_eid(Nelec) = 0
               jEC_eid(Nelec) = j
               jSC_eid(Nelec) = k
            else

               do 120 i = 1,nrclust         ! Cycle on CC clusters
                  if(nrsect(i).ne.isec)  goto 120

                  if( abs(ec_ihit(j)-nriec(i)) .gt. CCicoor) goto 120    ! I - coord in EC
                  s = 1.13*nrsegm(i) - 1.3
c                 ==============================> convert to parameters
                  if(abs(s-jscpad(k)).gt.0.1*CCscpn) goto 120       ! SC paddle number
c
                  if(Nelec .ge. maxeid0) then
c     ==========================> too many electrons !
                     write(CRMESS,*) 'Too many electrons  :',Nelec
                     CALL RECMES(CRNAME,'W',CRMESS)
                     go to 200
                  else
c
c ========   *******  Electron candidate is found !  ******  =====
c
                     Nelec = Nelec + 1
                     jeidsec(Nelec) = isec
                     jCC_eid(Nelec) = 0
                     jEC_eid(Nelec) = j
                     jSC_eid(Nelec) = k
                  end if
 120           continue                 ! Cycle on CC clusters
            endif                       ! matching with CC ?
 110     continue                       ! Cycle on SC clusters
 100  continue                          ! Cycle on EC clusters
c
 200  continue
c ============> filling BOS bank EID0 :  
c
      if(Nelec.eq.0) return
c
      j = mbank(iw,'EID0',0,NCOL,Nelec)
      do i = 1,Nelec
         k = j + NCOL*(i-1)
         iw( k + 1 )  =  jeidsec(i)
         iw( k + 2 )  =  jCC_eid(i)
         iw( k + 3 )  =  jEC_eid(i)
         iw( k + 4 )  =  jSC_eid(i)
      end do
c
      END
c
c-----------------------------------------------------------------------
