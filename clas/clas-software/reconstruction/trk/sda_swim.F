C=======================================================================
      SUBROUTINE sda_swim(isec,ipl_min,ipl_max,if_mat,vins)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Track swimming (traj. propagation) in magnetic
C-                         field. Calculates track points on detector planes.
C-
C-   Inputs  : isec    - current sector number                      
C-             ipl_min - first plane to crossed during swimming
C-             ipl_max - last plane to crossed during swimming
C-             if_mat  - when set to 1, Transport Matrix is calculated
C-                       during swimming
C-             vins(9) - Starting values {x,y,z,Px/P,Py/P,Pz/P,P,m,Q}
C-
C-   Outputs : Results stored in COMMON/sdaswim/
C-
C-   Controls:
C-
C-   Calls   : sda_helix, sda_icyl, sda_ipln, sda_trmat
C-
C-   Created   OCT-26-1990  Bogdan Niczyporuk
C-   Modified  AUG-2-1992   Billy Leung
C-             Nov.1997     F.Klein
C-
C-   Called by ana_trfit
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE

#include "trktcl_var.inc"
#include "sdaswim.inc"
*
*  Subroutine parameters
*  ---------------------
      INTEGER isec, ipl_min, ipl_max, if_mat
      REAL vins(9)
*
*  External
*  --------
      REAL    VMOD
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: sda_swim.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.12 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/04/21 00:52:02 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: sda_swim.F,v 1.12 2008/04/21 00:52:02 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='SDA_SWIM')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local variables
*  ---------------
      INTEGER i,j,ist,ip,ipl,ipp,iflag,iflagp,new,nstep,ncyl_min,icross
      REAL ECon, Binv, Bold, Dangle, sint, stot, s_first, del_s, x0(2)
      REAL vect(9), vout(9), xint(9), bfld(3), Zpos, step
      Integer ptg_flag
      Real st_line_dist
C
      DATA ECon/2.99792E-4/
      DATA del_s/60./
C
C
C SDA_SWIM begins
C
      IF(ipl_min.LE.0) GO TO 999
C
      Dangle = dpar_trk_zangle/ECon
C
C Buffer "vect" is used in DO loop
C
      DO i=1,9
        vect(i) = vins(i)
      ENDDO
C
C Initialise hit array
C
      DO i=ipl_min,ipl_max
        DO j=1,3
          sda_trp(j,i)=1000.
          sda_trp(j+3,i) = 0.
        ENDDO
        sda_trp(7,i) = 1000.
      ENDDO
C
C Initialise indices
C
      nstep = 1000./dpar_trk_zstep(1)
      new   = 1
      ipl   = ipl_min
      stot  = 0.
      icross= 0
      ptg_flag=0   ! don't neglect ptg field except if starting from beamline
      if(ipl.lt.4) ptg_flag = 1
C
C Do loop traces particle through detector
C
      DO 100 ist = 1,nstep
C
C Calculate a step size
        IF(vins(9).NE.0.) THEN
          Bold = VMOD(bfld,3)
          IF(dpar_trk_zstep(1).LT.dpar_trk_zstep(2)) THEN
            IF(Bold.LT.0.005) THEN
              Binv = 200.
            ELSE
              Binv = 1./Bold
            ENDIF
            step = Dangle*vect(7)*Binv
            IF(step.LT.dpar_trk_zstep(1)) then
              step = dpar_trk_zstep(1)
            ELSEIF(step.GT.dpar_trk_zstep(2)) then
              step = dpar_trk_zstep(2)
            ENDIF
          ELSE
            step = dpar_trk_zstep(1)
          ENDIF
        ELSE
          step = 10.
        ENDIF
C First step size have to smaller then a distance to the 1-st plane
        IF(ist.EQ.1) step = 0.01
C
C Call Helix
        CALL sda_helix(step,vect, vout,bfld,ptg_flag,isec)
        ip = ipl
C
C It is assumed here that a track never cross again the same layer,
C but may cross a few layers within the same (constant) step length.
C
C Find intersection with cylinder or plane
        ncyl_min = ndc_min + 12
C
 10     CONTINUE
        sint = step
        IF(ipl.LT.ndc_min) THEN                           ! Planes near targ.
          if(ipl.EQ.1) then
            CALL sda_ipln(isec,vect,vout,ipl, sint,xint,iflag)
          else
            if(ABS(vout(5)).GT.0.0001) then
              Zpos=vout(1)+(sda_pln(8,2,isec)-vout(2))*vout(4)/vout(5)
              if(Zpos.GT.st_Zpos(2,isec)) ipl = 3
              CALL sda_ipln(isec,vect,vout,ipl, sint,xint,iflag)
              if(iflag.eq.0.and.vect(1).gt.st_Zpos(4,isec).and.
     &             zpos.gt.st_zpos(4,isec).and.vect(2).gt.2*sda_pln(8,2,isec)) then
                 ipl=4               ! skip start counter for tracks from upstream
              endif
            endif
          ENDIF
        ELSE IF(ipl.GE.ndc_min .AND. ipl.LT.ncyl_min) THEN   ! Reg.1
          IF(vect(1).LT.0.) THEN
            if(st_line_dist.ge.sda_pln(8,4,isec)) ptg_flag = 0
            CALL sda_ipln(isec,vect,vout,ipl, sint,xint,iflag)
            if(iflag.eq.0 .and. vout(1).gt.0) then
               CALL sda_icyl(isec,vect,vout,ipl, sint,xint,iflag)
            endif
          ELSE
            if(st_line_dist.ge.sda_pln(7,4,isec)) ptg_flag = 0
            CALL sda_icyl(isec,vect,vout,ipl, sint,xint,iflag)
            if(iflag.eq.0 .and. vout(1).lt.0) then
               CALL sda_ipln(isec,vect,vout,ipl, sint,xint,iflag)
            endif
          ENDIF 
        ELSE IF(ipl.GE.ncyl_min .AND. ipl.LE.ncc_max) THEN   ! Reg.2,3 & CC
          if(ipl.gt.39) ptg_flag = 1
cc          if(ipl.gt.14) ptg_flag = 1
          CALL sda_icyl(isec,vect,vout,ipl, sint,xint,iflag)
        ELSE IF (ipl_max.GE.nsc_min) THEN                    ! SC & EC
          iflag = 0
          DO ipp = nsc_min,ipl_max
            CALL sda_ipln(isec,vect,vout,ipp, sint,xint,iflagp)
            IF(iflagp.EQ.1) THEN
              DO i = 1,6
                sda_trp(i,ipp) = xint(i)
              ENDDO
              sda_trp(7,ipp) = stot + sint
              icross = icross + 1
              IF(icross.EQ.1) s_first = sda_trp(7,ipp)
            ENDIF
          ENDDO
          IF(icross.EQ.0) THEN
            GO TO 20
          ELSE
            IF((stot-s_first).GT.del_s) GO TO 999
          ENDIF 
        ELSE
        ENDIF
C
        IF(iflag.EQ.1) THEN
C Track crossed a cylinder or plane
c          IF(if_mat.GT.0 .AND. ipl.LE.ndc_max)
          IF(if_mat.GT.0 .AND. ipl.LE.nsc_max)
     1       CALL sda_trmat(new,vect,xint,sint,bfld,ipl,iflag)
          DO i = 1,6
            sda_trp(i,ipl) = xint(i)
          ENDDO
          sda_trp(7,ipl) = stot + sint
          do i=1,3
             sda_trp(7+i,ipl) = bfld(i)
          enddo 
c          IF(ipl.EQ.2 .AND. xint(1).LT.-10.) THEN
          IF(ipl.EQ.2 .AND. xint(1).LT.st_Zpos(3,isec)) THEN
            ipl = ipl + 2
          ELSE
            ipl = ipl + 1
          ENDIF
C Check if last layer has been reached
          IF(ipl.GT.ipl_max) GO TO 999
C Check next layer crossing within the same step
          GO TO 10
        ENDIF
C
 20     CONTINUE
        IF(if_mat.GT.0 .AND. ipl.LE.nsc_max)
     1     CALL sda_trmat(new,vect,vout,step,bfld,0,0) 
        stot = stot + step
        if(vout(5).lt.-0.5) goto 999 
        IF(stot.gt.50. .and. vout(2).LE.0.) goto 999
        if(stot.GT.1400.) GO TO 999
        DO j=1,9
          vect(j) = vout(j)
        ENDDO
        st_line_dist=sqrt(vect(1)**2+vect(2)**2+vect(3)**2)+2.*sint
 100  CONTINUE                      ! End loop over Nstep
C
 999  CONTINUE
C
      RETURN
      END








