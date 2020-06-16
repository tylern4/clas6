CDECK  ID>, CCVPHO.
      SUBROUTINE CCVPHO(X_IN,d_in,ncc_i,jhit,ncc_o,s_plus,x_out,d_out)
c     ================================================================
c
c_begin_doc
c  RCS ID string
c  $Id: ccvpho.F,v 1.4 1999/02/05 00:18:48 vlassov Exp $
c
c  Documentation for subroutine
c               CCVPHO(X_IN,d_in,ncc_i,jhit,ncc_o,s_plus,x_out,d_out)
c               =====================================================
c
c
CA)========================================(begin of short description)
C       PHOTON tracing inside Cherenkov counter
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
c               The task of this subroutine is to trace Photons
c               from input point (x_in,d_in) up to the point of
c               exit (x_out,d_out).
c               =================================================
C
C----------------------------------------------------------------------
C    Called by: CCSTEP
C
C    Input arguments :
C          x_in(3) - input point coordinats in B_K_D system
c          d_in(3) - unit vector of initial direction
c          ncc_i   - number of CC - counter
c
C    Input/output arguments : none
C
C    Output arguments:
c    In case if output point is not on the PMT window jhit=0;
c       else jhit=1 - left   PMT , jhit=2 - right  PMT (x>0)
c       ncc_o :  # of CC - hit (it can differ from ncc_i)
c       s_plus - the total lentgh from initial point to PMT (cm)
C       x_out(3) - output point coordinats in B_K_D system
c       d_out(3) - unit vector of final direction
C
C    Side effects (other variables/banks modified, etc.)
C-
C-   Created     4-MAY-1994   Alexander V. Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C-
CC)-------------------------------------------(end of long description)
C----------------------------------------------------------------------
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
#include "gconsp.inc"
#include "ccp.inc"
#include "gcflag.inc"
#include "ccph.inc"
c
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER ncc_i, ncc_o, jhit
      REAL x_in(3), x_out(3), d_in(3), d_out(3), s_plus
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c                                                                     rcs
c  RCS information:                                                   rcs
c                                                                     rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccvpho.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/02/05 00:18:48 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/ 
     1'$Id: ccvpho.F,v 1.4 1999/02/05 00:18:48 vlassov Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccvpho')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                     rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      REAL  vdot,vdist
      external  vdot,vdist
C
      INTEGER j,k,NPT,NCC,ICH,LR,LRP,IWC,NWC
      integer nin, numwc, kwc(3), MTYPE, ncc_m
      integer IRES, IRES0, IRES1, IRES2, IRES3, IRES4, IRES5
C
      real x(3),d(3), D0(3),SPHO, uwc(3),vwc(3)
      REAL S,S0,S1,S2,S3,S4,S5,S6
      REAL RS(3),R(3),R0(3),R1(3),R2(3),R3(3),R4(3),R5(3),R6(3)
      REAL d1,d2,d3,grn(7)
      REAL small,large,reflef,vsmall, Z(3)
      REAL PWM(3),RMR(3),DMR(3),sm1,sm2,sm3,umr(3),vmr(3)
C-
      data vsmall,small,large/ 0.000001, 0.001, 1000./
c
      data Z/0.,0.,0./
      data reflef / 0.8/
C
CE)===============================================(end of declarations)
c_end_var
c
c  executable code for routine CCVPHO
c------------------------------------------------------------------------
c
c     Initial settings :
c
      SPHO = 0.
      jhit = 0
      NPT = 0           ! current number of inersections
      call grndm(grn,7)
      call ucopy(x_in,x,3)
      call ucopy(d_in,d,3)
c
      if(x_in(1).gt.small) then
        LR = 1
      else if(x_in(1).lt.-small) then
        LR = -1
        x(1) = -x_in(1)
        d(1) =  -d_in(1)
      else
        if(d_in(1).lt.0.) then
          LR = -1
          x(1) = -x_in(1)
          d(1) =  -d_in(1)
        else
          LR = 1
        end if
      end if
c
      s = th(ncc_i) + raddeg*atan(x(3)/x(2))
      if(s.lt.thmin(ncc_i)) then    ! Initial point in the smaller CC #
c
c   TO NEW CC Reference System :
c
        ICH = ncc_i - 1
        if(ICH.ge.1) then
          call CC1TO2(ncc_i,x,d,ICH,RS,D)
c
        else
          go to 300   ! Point is out of the volume !
        end if
c
      else if(s.gt.thmin(ncc_i + 1)) then   ! Point in the bigger CC #
c
c   TO NEW CC Reference System :
c
        ICH = ncc_i + 1
        if(ICH.le.18) then
          call CC1TO2(ncc_i,x,d,ICH,RS,D)
c
        else
          go to 300   ! Point is out of the volume !
        end if
c
      else
c
        ICH = ncc_i
        call ucopy(x,RS,3)
c
      end if
c
      ncc_o = ICH     ! current number of CC-count
      call ccdefpar(ICH)
c
      LRP=LR
      IWC = 0
      NWC = 0
      nin = 0
c
C *************   now rs - point of radiation and d - direction.
c
c ====   Are we inside WC or not ?  ===========
c
      kwc(1) = ICH
c
      if(ICH.eq.1) then
        if(rs(3).lt.0.) then
          numwc = 1
        else
          numwc  = 2
          kwc(2) = ICH + 1
        end if
      else if(ICH.eq.18) then
        if(rs(3).lt.0.) then
          numwc = 2
          kwc(2) = ICH - 1
        else
          numwc  = 1
        end if
      else
        numwc = 2
        if(rs(3).lt.0.) then
          kwc(2) = ICH - 1
        else
          kwc(2) = ICH + 1
        end if
      end if
c
      do 10 j = 1,numwc
        ncc = kwc(j)
c
        if(ncc.eq.ICH) then
c       =================== we are in the right Coordinate System
c
          call ucopy(RS,R,3)
          call ucopy(D,D0,3)
c
        else
c
c   TO NEW CC Reference System :
c
          call CC1TO2(ICH,RS,D,ncc,R,D0)
c
        end if
c
        call CCDEFPAR(ncc)
c =======
c         SUM of distances from point to WC window plane and
c         PMT plane = heigt +/- small - yes
c =======
        call ccdpln(R,P3,s1)
        call ccdpln(R,P4,s2)
        s = abs(abs(s1) + abs(s2) - hwcpmt(ncc))
        if (s.gt.small) go to 10                  ! NO
c
c   TO WC Reference System :
c
        call CCTOWC(ncc,R,D0,uwc,vwc)
c
c =======
c         SUM of distances from point to WC bounding planes
c =======
        call ccdpln(uwc,pwc1,s1)
        call ccdpln(uwc,pwc2,s2)
        s = abs(abs(s1) + abs(s2) - dplwc(ncc))
        if (s.gt.small)      go to 10             ! NO
        if(abs(s1).le.small) go to 10             ! NO
        if(abs(s2).le.small) go to 10             ! NO
C----------------------------------------------------------------
c
c  It looks like that we are inside WC .
c  Now we are to check this once more.
c
        call CCWCCR(uwc,vwc,PW,IRES,S1,R1)
        if(IRES.eq.1) then           !  Yes, we are inside WC !!
c
          IWC = 1      ! We are inside WC
          NIN = 1      ! That is not initial state
          NWC = ncc    ! WC #
          ICH = ncc    ! CC #
          call ucopy(R,RS,3)
          call ucopy(D0,D,3)
****      uwc and vwc - correspondingly point and direction in WC system
          go to 210
c
        end if
c
   10 continue
c     ========     NO, we are NOT inside WC and so can enter into it
c                       only thrue WINDOW (WC window) !
      nin = 1
      IWC = 0
c
  100 CONTINUE
c   ==========  MAIN cycle
      NPT=NPT+1
      call ucopy(RS,rl(1,npt,ig),3)
      NCL(NPT,IG)=ICH
      if(npt.ge.mref) then
c        write(CRMESS,*) 'Too many new points for photon tracing!',npt
c        call recmes(CRNAME,'I',CRMESS)
        NPT = mref - 1
        go to 330
      end if
c
c --- WC test ---  are we to cross the WC window?  ( 2 - 3 attempts !)
c
c ====== Determination of CC # to be checked.
c
      kwc(1) = ICH
c
      if(ICH.eq.1) then
        numwc  = 2
        kwc(2) = ICH + 1
      else if(ICH.eq.18) then
        numwc = 2
        kwc(2) = ICH - 1
      else
        numwc = 3
        kwc(2) = ICH - 1
        kwc(3) = ICH + 1
      end if
c
      do 110 j = 1,numwc
c     ==================
c
        ncc = kwc(j)
c
        if(ncc.eq.ICH) then
c       =================== we are in the right Coordinate System
c
          call ucopy(RS,R,3)
          call ucopy(D,D0,3)
c
        else
c
c   TO NEW CC Reference System :
c
          call CC1TO2(ICH,RS,D,ncc,R,D0)
c
C------------------------------------------------------------
c
        end if
c
        call CCDEFPAR(ncc)
c
c       Photon direction
c
        s = vdot(WCC,D0,3)
 
       if (s.le.0.) go to 110   !  Even when reached WC thrue the window
c                                   you should keep the right direction.
c
C --- Check crossing with entering plane of Winston Cone
c
c
        CALL CCVCRPL(R,D0,P3,IRES,S,R)
C----------------------------------------------------------------
        if(IRES.le.0) go to 110      !  NO cross in positive direction
c ======
c       Distance between crossing point and WC window center.
c ======
        s = vdist(R,WCC,3) 
        if(s.gt.wcer(ncc)) go to 110  !  NO cross with WC window.
c
c =======
c       SUM of distances from point to WC bounding planes
c =======
        call ccdpln(R,P8,s1)
        call ccdpln(R,P9,s2)
        s = abs(abs(s1) + abs(s2) - dplwc(ncc))
        if (s.gt.small)      go to 110    !  NO cross with WC window
        if(abs(s1).le.small) go to 110    !  NO cross with WC window
        if(abs(s2).le.small) go to 110    !  NO cross with WC window
c
C----------------------------------------------------------------
c
c ***** Yes ! The photon reaches the WC window al least ! *******
c
        IWC = 1      ! We are inside WC
        NWC = ncc    ! WC #
        ICH = ncc    ! CC #
        call ucopy(R,RS,3)     ! initial point in CC system
        call ucopy(D0,D,3)     ! initial direction in CC system
*
*******************************************
*
        go to 200
c
C----------------------------------------------------------------
c
  110 continue
c     ========     NO, photon DOES NOT cross the WC window
      IWC = 0
c
      call CCDEFPAR(ICH)
c
  111 continue
c     ========
c     ========     NO, photon DOES NOT cross the WC window  -
c                  let's try to reflect it from any mirror !
c
c     So, let's determin the mirror type.
c
      MTYPE = 1                   !   Elliptical mirror
      If(D(2).lt.0.) MTYPE = 2    !   Hyperbolic mirror
c
c           PREFACE
c   There are only five ways to the success now:
c      1. To reach the mirror
c      2. To cross the middle plane
c      3. To cross (or reflect off) the down-plane
c      4. To cross (or reflect off) the up-plane
c      5. To reflect off the cylindrical mirror.
c
c   All other ways result in the absorption.
c   Let's find the nearest one.
c
C --- Check crossing with the middle plane
      CALL CCVCRPL(RS,D,P00,IRES0,S0,R0)
      if(ires0.le.0) s0 = large
c
C --- Check crossing with the down-plane
      CALL CCVCRPL(RS,D,P6,IRES1,S1,R1)
      if(ires1.le.0) s1 = large
      if(S1.le.small) s1 = large
c
C --- Check crossing with the up-plane
      CALL CCVCRPL(RS,D,P7,IRES2,S2,R2)
      if(ires2.le.0) s2 = large
      if(S2.le.small) s2 = large
c
      if(MTYPE.eq.1) then
C --- Check crossing with the elliptical mirror
        call CCROSE(RS,D,PE,IRES3,S3,R3)
      else
C --- Check crossing with the hyperbolic mirror
        call CCROSH(RS,D,PH,IRES3,S3,R3)
c
      end if
C
      if(ires3.le.0) then
        call vadd(RS,D,RS,3)
        go to 300                     ! NO hit
      end if
c
c *******   NEW STUFF   **********

c
c --- WC cylindrical mirrors test 
c
c ====== Determination of CC # to be checked.
c
      kwc(1) = ICH
c
      if(ICH.eq.1) then
        numwc  = 2
        kwc(2) = ICH + 1
      else if(ICH.eq.18) then
        numwc = 2
        kwc(2) = ICH - 1
      else
        numwc = 3
        kwc(2) = ICH - 1
        kwc(3) = ICH + 1
      end if
c
      S = large
      ncc_m = 0
c
      do 112 j = 1,numwc
c     ==================
        ncc = kwc(j)
        if(ncc.eq.ICH) then
c       =================== we are in the right Coordinate System
          call ucopy(RS,R,3)
          call ucopy(D,D0,3)
        else
c
c   TO NEW CC Reference System :
c
          call CC1TO2(ICH,RS,D,ncc,R,D0)
        end if
        call CCDEFPAR(ncc)
c
C --- Check crossing with the WC cylindrical surface
c
c   TO WC Reference System :
c
        call CCTOWC(ncc,R,D0,uwc,vwc)
C----------------------------------------------------------------
****   uwc and vwc - point and direction in WC system
c
c ***  That is a new start point inside WC ***
c
c  Check cross with the WC cylindrical mirror.
c
        PWM(1) = -1./(wcer(ncc)*wcer(ncc))
        PWM(2) = -1./(wcer(ncc)*wcer(ncc))
        PWM(3) =  0.
c
        call CCWCMCR(uwc,vwc,PWM,IRES,S5,R5,S6,R6)
        if(IRES.eq.0) then
          S5 = large
          go to 112           !  NO cross
        end if
c
c   From WC to CC Reference System :
c
        if(IRES.eq.2) then 
          call ucopy(R6,R5,3)
          S5 = S6
        end if          
        if(S5.ge.S)then
          go to 112
        end if
*
        if(R5(2).lt.0.) then 
          S5 = large
          go to 112   !  Wrong cross_section
        end if
*
c       SUM of distances from point to WC bounding planes
c       =================================================
        call ccdpln(R5,pwc1,sm1)
        call ccdpln(R5,pwc2,sm2)
        sm3 = abs(abs(sm1) + abs(sm2) - dplwc(ncc))
        if (sm3.gt.small) then
          s5 = large
          go to 112                  ! NO
        end if
c
        call CCFRWC(ncc,R5,vwc,R,D0)
        if(R(2).ge.Y22(ncc).or.R(2).lt.Y21(ncc)) then
          s5 = large
          go to 112
        end if
c
        s = s5
        ncc_m = ncc
        call ucopy(R5,umr,3)
        call ucopy(vwc,vmr,3)
        call CCFRWC(ncc_m,R5,vwc,RMR,DMR)
c
C----------------------------------------------------------------
c
  112 continue
c
      if(S.lt.large) then
        s5 = s
        call cc1to2(ncc_m,RMR,DMR,ICH,R5,D0)
      end if
      call CCDEFPAR(ICH)
c
c *******   NEW STUFF   **********
c
      s = min(s0,s1,s2,s3,s5)
c
      if(s.ge.large) then
        go to 300
      end if
c
c           PREFACE
c   It doesn't mean, that the photon can reached
c   each of these points easily, -
c   now we are to check that there no any obstacle
c
c  ----   TESTs  --------
c
c  1.
c ---- Check crossing with the PMT side wall
      CALL CCVCRPL(RS,D,P1,IRES4,S4,R4)
c
      if(     ires4.eq.1
     &  .and. S4.lt.S
     &  .and. R4(2).lt.Y12(ICH)
     &  .and. R4(2).gt.Y11(ICH) ) then
        if(P1(1)*vdot(D,P1,3).gt.0.
     &    .or.     D(1)      .gt.0.) then
          call ucopy(R4,RS,3)
          go to 300            ! NO hit
        end if
      end if
c
c----  test 1  passed  ---
c
c  2.
c ---- Check crossing with the Magnetic Shield walls
c
      do k = 1,4       ! Wall #
c
        CALL CCVCRPL(RS,D,plms(1,k),ires4,s4,r4)
c
        if( ires4.eq.1 .and. S4.lt.S) then
c
          call ccdln2(R4,pcms(1,1,k),pcms(1,2,k),d1)
          call ccdln2(R4,pcms(1,3,k),pcms(1,4,k),d2)
          d3 = vdist(pcms(1,2,k),pcms(1,3,k),3)
c
          if(abs(d3 - d2 - d1).le.small) then
c
            call ccdln2(R4,pcms(1,2,k),pcms(1,3,k),d1)
            call ccdln2(R4,pcms(1,1,k),pcms(1,4,k),d2)
            d3 = vdist(pcms(1,1,k),pcms(1,2,k),3)
c
            if(abs(d3 - d2 - d1).le.small) then     !  NO hit !
c
              go to 300          ! NO hit
c
            end if
c
          end if
c
        end if
c
      end do
c
c----  test 2  passed  ---
c
c *****   Any other materials ?   *****
c
c ------  All tests passed !  -------
c
      SPHO  = SPHO+S
c
      if(abs(s0-s).lt.vsmall) go to 120    !  middle plane
      if(abs(s1-s).lt.vsmall) go to 130    !  down-plane
      if(abs(s2-s).lt.vsmall) go to 140    !  up-plane
      if(abs(s3-s).lt.vsmall) go to 150    !  ellipt. (hyp.) mirror
      if(abs(s5-s).lt.vsmall) go to 160    !  cylindrical mirror
      CRMESS = 'SORRY !?'
      call recmes(CRNAME,'W',CRMESS)
*
  120 continue
c
c ==========  Middle plane  ========
c
      call ucopy(R0,RS,3)
      RS(1) = -RS(1)
      D(1)  = -D(1)
      LRP   = -LRP
c
      go to 100  ! New point
c
  130 continue
c
c ==========  Down plane  ========
c
      call ucopy(R1,RS,3)
      if(ICH.eq.1)  then
c
c   ***  NO  ***  Reflection from plane # 1  ***
c
        go to 300
c
      else
c
c   TO NEW CC Reference System :
c
        ncc = ICH - 1
        call CC1TO2(ICH,RS,D,ncc,RS,D)
c
        ICH = ncc
        call CCDEFPAR(ICH)
c
      end if
      go to 100  ! New point
c
  140 continue
c
c ==========  Up plane  ========
c
      call ucopy(R2,RS,3)
      if(ICH.eq.18)  then
c
c   ***  NO  ***  Reflection from plane # 18  ***
c
        go to 300
c
      else
c
c   TO NEW CC Reference System :
c
        ncc = ICH + 1
c
        call CC1TO2(ICH,RS,D,ncc,RS,D)
c
        ICH = ncc
        call CCDEFPAR(ICH)
c
      end if
c
      go to 100  ! New point
c
  150 continue
c
c ==========  MIRROR  ========
c
      if(MTYPE.eq.1) then         ! Elliptical mirror
        if(R3(1).lt.XE_PL) then   ! Flat part
          call CCVCRPL(RS,D,PE_PL,IRES3,S3,R3)
c
c   ***  Reflection from plane PE_PL  ***
c
          call ucopy(R3,RS,3)
          if(grn(1).gt.reflef) go to 300
          call CCREFLPL(D,PE_PL,D)
c
        else if(R3(1).gt.X12(ICH)) then
          call ucopy(R3,RS,3)
          go to 300               ! NO hit : absorption on the frame
c
        else
c
c   ***  Reflection from the elliptical mirror  ***
c
          call ucopy(R3,RS,3)
          if(grn(2).gt.reflef) go to 300
          call CCREFLOC(RS,D,PE,D)
        end if
      else
c
c   ***  Reflection from the hyperbolic mirror  ***
c
        call ucopy(R3,RS,3)
        if(grn(3).gt.reflef) go to 300
        call CCREFLOC(RS,D,PH,D)  ! Hyperbolic mirror
      end if
c
      if(MTYPE.eq.1) then
c     ===================
c
c     There are NO reason to check if photon cross
c     the WC window after reflection off the elliptical mirror
c
        NPT=NPT+1
        call ucopy(RS,rl(1,npt,ig),3)
        NCL(NPT,IG)=ICH
        if(npt.ge.mref) then
          write(CRMESS,*) 'Too many new points for photon tracing!'
     &    ,npt
          call recmes(CRNAME,'I',CRMESS)
          NPT = mref - 1
          go to 330
        end if
c
        go to 111
      end if
c
      go to 100  ! New point
c
  160 continue
c ============
c
c ======  CYLINDRICAL  MIRROR  ========
c
      ICH = ncc_m
      call ucopy(R5,RS,3)
      call ucopy(D0,D,3)
c
c   ***  Reflection from the cylindrical mirror  ***
c
      if(ncc_m.le.0 .or.ncc_m.gt.18) then
        write(CRMESS,*) 'WRONG ncc_m !!!',ncc_m
        call recmes(CRNAME,'E',CRMESS)
      end if
      PWM(1) = -1./(wcer(ncc_m)*wcer(ncc_m))
      PWM(2) = PWM(1)
      PWM(3) =  0.
      call CCWCRF(umr,vmr,PWM,vwc)   !  Reflection off WC
c
c   From WC to CC Reference System :
c
      call CCFRWC(ncc_m,umr,vwc,R,D0)
c
      call cc1to2(ncc_m,R,D0,ICH,RS,D)
      if(grn(4).gt.reflef) go to 300       
c
      go to 100  ! New point
c
  200 CONTINUE
c
c*************************************************************
c       ========  Ray tracing inside WC  =========
c*************************************************************
c
c   TO WC Reference System :
c
      call CCTOWC(ICH,RS,D,uwc,vwc)
c
C----------------------------------------------------------------
C-
****   uwc and vwc - point and direction in WC system
c
c ***  That is a new start point inside WC ***
c
  210 continue
c     ========
c -------------------------------------------------------------------
      NPT=NPT+1
      call ucopy(RS,rl(1,npt,ig),3)
      NCL(NPT,IG)=ICH
      if(npt.ge.mref) then
        write(CRMESS,*) 'Too many new points for photon tracing!',npt
        call recmes(CRNAME,'I',CRMESS)
        NPT = mref - 1
        go to 330
      end if
c
c -------------------------------------------------------------------
c
c  Check cross to the WC.
c
      call CCWCCR(uwc,vwc,PW,IRES,S1,R1)
      if(IRES.ne.1) then           !  (?!)
        go to 300                      ! NO hit
      end if
c
C --- Check crossing with the PMT-plane
      CALL CCVCRPL(RS,D,P4,IRES2,S2,R2)
      if(ires2.le.0) s2 = large
c
C --- Check crossing with the WC window plane
      CALL CCVCRPL(RS,D,P3,IRES3,S3,R3)
      if( abs(s3).le.small
     &  .and. vwc(3) .gt.0.) then   ! We on the WC window surface
        s3 = large
      end if
      if(ires3.le.0) s3 = large
c
C --- Check crossing with WC bound. plane (8)
      CALL CCVCRPL(RS,D,P8,IRES4,S4,R4)
      if(ires4.le.0) s4 = large
      if(abs(S4).lt.small) s4 = large
c
C --- Check crossing with WC bound. plane (9)
      CALL CCVCRPL(RS,D,P9,IRES5,S5,R5)
      if(ires5.le.0) s5 = large
      if(abs(S5).lt.small) s5 = large
c
c -------------------------------------------------------------------
c
      s = min(s1,s2,s3,s4,s5)
      if(s.ge.large) then
        write(CRMESS,*) 'SORRY ! MIN distance to surfaces is',s,'   ?!'
        call recmes(CRNAME,'W',CRMESS)
      end if
c
c ===================================
c
      SPHO  = SPHO+S
c
      if(abs(s2-s).lt.vsmall) then   !  ***  Hit!  ***
        call ucopy(R2,RS,3)
        go to 400        !  ***  Hit!  ***
c
      else if(abs(s1-s).lt.vsmall) then  ! Cross with WC
c
        call ucopy(R1,uwc,3)
C------------------------------------------------------------
c
c   ***  Reflection from the Winston cone  ***
c
        call CCWCRF(uwc,vwc,PW,vwc)   !  Reflection off WC
c
c   From WC to CC Reference System :
c
        call CCFRWC(ICH,uwc,vwc,R,D0)
c
        call ucopy(D0,D,3)
        call ucopy(R,RS,3)
c
        if(grn(5).gt.reflef) go to 300
        go to 210
c
      else if(abs(s3-s).lt.vsmall) then  !  OUT from WC ?
c
        IWC = 0
        call ucopy(r3,RS,3)
        go to 111
c
      else if(abs(s4-s).lt.vsmall) then  !  Reflection off p8
c
        call ucopy(R4,RS,3)
        if(grn(6).gt.reflef) go to 300
c
        call CCREFLPL(D,P8,D0)
        call ucopy(d0,d,3)
c
      else if(abs(s5-s).lt.vsmall) then  !  Reflection off P9
c
        call ucopy(R5,RS,3)
        if(grn(7).gt.reflef) go to 300
c
        call CCREFLPL(D,P9,D0)
        call ucopy(d0,d,3)
c
      end if
      go to 200
c
  300 continue
c
c ============  NO HIT
c
      NPT=NPT+1
      call ucopy(RS,rl(1,npt,ig),3)
      NCL(NPT,IG)=ICH
      if(npt.ge.mref) then
        NPT = mref
      end if
c
      jhit  = 0
      ncc_o = ICH
      s_plus = SPHO
      call ucopy(RS,x_out,3)
      call ucopy( D,d_out,3)
      nref(ig) = NPT
      ihnc(ig) = 0
c
      return
c
  330 continue
c ============  Point is out of the volume
c
      jhit = 0
      ncc_o = ncc_i
      s_plus = 0.
      call ucopy(x_in,x_out,3)
      call ucopy(d_in,d_out,3)
      return
c
  400 continue
c
c ============   HIT !!!
c
      NPT=NPT+1
      call ucopy(RS,rl(1,npt,ig),3)
      NCL(NPT,IG)=ICH
      if(npt.ge.mref) then
        NPT = mref
      end if
C
      ncc_o = ICH
      s_plus = SPHO
      call ucopy(RS,x_out,3)
      call ucopy( D,d_out,3)
      nref(ig) = NPT
c
      if(LRP.eq.1) then
        jhit  = 1
        ihnc(ig) = ICH
      else
        jhit  = 2
        ihnc(ig) = -ICH
      end if
c
      return
      end
c ======================================================
