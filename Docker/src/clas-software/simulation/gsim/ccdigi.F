CDECK  ID>, CCDIGI.
      SUBROUTINE CCDIGI
c
c_begin_doc
c  RCS ID string
c  $Id: ccdigi.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $
c
c  Documentation for subroutine CCDIGI
c
CA)========================================(begin of short description)
C Digitization of the Cherenkov counter responce
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods : Digitization of the Cerenkov counters
C
C    Called by:
C
C    Input arguments : none
C
C    Input/output arguments : none
C
C    Output arguments: none
C
C    Side effects (other variables/banks modified, etc.)
C    DIGI bank is filled 
C
C    Created:   09-FEB-1994  A.Vlassov
C    Modified:  02-MAY-1994  A.Vlassov
C    Modified:  12-JUN-1995  A.Vlassov
C    Modified:  Alexander Vlassov  Tue Aug  8 16:44:35 EDT 1995
CC)-------------------------------------------(end of long description)
c
c_end_doc
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
c_begin_inc
c  include files :
c  ---------------------
#include "ccph.inc"
#include "gconsp.inc"
#include "ccdig.inc"
#include "gcflag.inc"
#include "gcbank.inc"
#include "cccom.inc"
c  ---------------------
c_end_inc
c
c_begin_var
c                                                                         rcs
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:                                                       rcs
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccdigi.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:06 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdigi.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccdigi')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
C  External references
c
c  Local User defined variables:
c  -----------------------------
      INTEGER i,j,ks,kc,itc
      integer numvs(10),kdigi(3),ithreshold
      integer iset,idet,ntrack,idig
      integer nv,nbitsv(10),idtyp,nwhi,nwdi
      integer itrack(mhtrk)
      integer k
      character*4 chnmsv(10)
c
C-----------------------------------------------------------------------
C
      DATA ithreshold/1/
C
C  =================   END OF DECLARATIONS  ====================
C
c_end_var
c
c  executable code for routine CCDIGI:
c
      n_hits_CC = 0
      IF(ig.GE.1) THEN   ! There is something for work!
C
C ---   CCDIGI filling:
C
        call vzero(n_SECT_CC,max_n_hit_CC)
        call vzero( n_CC_hit,max_n_hit_CC)
        call vzero( I_TDC_CC,max_n_hit_CC)
        call vzero( I_ADC_CC,max_n_hit_CC)
        call vzero(I_TRACK_N,max_n_hit_CC)
C
        DO 200 i = 1,ig
C       ===============
          if(n_hits_CC.ge.max_n_hit_CC) then
            go to 200
          end if
          if(ihnc(i).eq.0) go to 200              ! NO hit
C
          kc  = ihnc(i)
          ks  = ihsc(i)
          itc = CM_TO_CH * tdc(i) + 0.5
c
          if(n_hits_CC.eq.0) then
c
            n_hits_CC = 1
            n_SECT_CC(n_hits_CC) = ks
            n_CC_hit (n_hits_CC) = kc
            I_TDC_CC (n_hits_CC) = itc
            I_ADC_CC (n_hits_CC) = 1
            ntrack = 1
            itrack(ntrack)       = njtr(i)
            I_TRACK_N(n_hits_CC) = njtr(i)
c
          else
c
            do j = 1, n_hits_CC
c
              if (n_SECT_CC(j).eq.ks
     &        .and.n_CC_hit(j).eq.kc) then     ! The same CC #
                I_ADC_CC (j) = I_ADC_CC (j) + 1
                if(itc.le.I_TDC_CC(j)) I_TDC_CC (j) = itc
                go to 200
              end if
c
            end do
c
c           ========>                            ! New CC #
c
            n_hits_CC = n_hits_CC + 1
            n_SECT_CC(n_hits_CC) = ks
            n_CC_hit (n_hits_CC) = kc
            I_TDC_CC (n_hits_CC) = itc
            I_ADC_CC (n_hits_CC) = 1
            I_TRACK_N(n_hits_CC) = njtr(i)
c
            do j = 1,ntrack
              if(itrack(j).eq.njtr(i)) go to 200  ! No new track
            end do
            if(ntrack.ge.mhtrk) go to 200
            ntrack = ntrack + 1
            itrack(ntrack) = njtr(i)
c
          end if
C
  200   CONTINUE
C
C       STORE DIGI
C       ==========
C return iset for 'CC  ' and idet for 'CCVO'
C
        call GFDET('CC  ','CCVO',
     &  nv,chnmsv,nbitsv,idtyp,nwhi,nwdi,iset,idet)
c
        IF(n_hits_CC.ge.1) then
C
          DO i=1,n_hits_CC
c
C------ sector number
            numvs(1) = n_SECT_CC(i)
C------ CC_section number
            j = n_CC_hit (i)
            numvs(2) = iabs(j)
C------ Right - Left section #
            if(j.gt.0) then
              numvs(3) = 1
            else
              numvs(3) = 2
            end if
C------ CC number 1-36 odd /even : left - right
            kdigi(1) = 2*numvs(2) - numvs(3) + 1
C------ I_TDC
            kdigi(2) = I_TDC_CC(i)
C------ I_ADC
            kdigi(3) = I_ADC_CC(i)
C===============================
C
            IF(kdigi(3).GE.ithreshold) THEN
              CALL GSDIGI(iset,idet,itrack,ntrack,numvs,kdigi,idig)
              IF(idig.LE.0) THEN
                CRMESS = 'Digitization has not been stored.'
                call recmes(CRNAME,'E',CRMESS)
              END IF
            ENDIF
C 
          ENDDO
        else
c       ==============>   NO HIT event
C**********************************************************
C
        end if
c     =======================>  end of DIGI
      END IF
C
C**********************************************************
c
  999 continue
c
      RETURN
      END
