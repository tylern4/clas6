C======================================================================
      SUBROUTINE ana_trfit(isec,ilnk,Level,ipmin,svin,chifit)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a fit of the track candidate.
C-
C-
C-   Inputs  : isec      - Sector number being analyzed 
C-             ilnk      - Track candidate number in Sector "isec"
C-             Level     - Level of analysis
C-             ipmin     - start plane for fit
C-             svin(9)   - Starting (final) values of the track parameters
C-
C-   Outputs : svin(9)   - fitted (corrected) track parameters
C-                         {x,y,z,Px/P,Py/P,Pz/P,p,m,Q} at the target position.
C-             chifit    - Chi Square of the fit
C-
C-   Controls:             See DATA card "TFIT"
C-
C-
C-   Library belongs    : libana.a
C-
C-   Calls              : sda_swim, ana_mxinv, ana_mvprod, user_trk
C-
C-   Created   30-OCT-1990   Bogdan Niczyporuk
C-   Modified  22-JUNE-1992  Billy Leung (IMPLICIT NONE implemented)
C-             Nov.1997      F.Klein  (6-fold geometry & remove 'sdakeys')
C-
C-   Called by ANA_PRFIT, ANA_FIT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER isec, ilnk, Level, ipmin
      REAL svin(9), chifit
*
*  Include Files
*  -------------
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
#include "sdaswim.inc"
#include "anapatt.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_trfit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.26 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:25 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_trfit.F,v 1.26 2008/03/16 04:33:25 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_TRFIT')
      PARAMETER (CRAUTH='Franz Klein')

C
C Constants
      REAL big, raddeg, pi, piby2
      integer MAX_WIRE
      parameter(MAX_WIRE = 192)
C
C External functions
      REAL    VDOT
C
C Local variables
      INTEGER i, j, k,id, il, iwr, is,ipl, ilay, iter, iterl
      INTEGER ifirst, ilmin, ilmax
      INTEGER maxiter, mypar_vxconstr
      integer mypar_SCconstr, my_schit, my_scplane,isec_last
      integer ncol_sc, nd_sc, nhit_sc, MLINK, ind_scr, max_reinit
      REAL ex(3),ez(3), u0(3),u(3), svin0(9)
      REAL dtrsp(5,ndc_max), vx(ndc_max), weight(ndc_max)
      REAL alfa(5,5),beta(5),dx(5), alfaw(5,5),errx(5)
      REAL wwd(36),ems(36),inv_beta2, pmin_cut        
      REAL diff(3), temp(3), diff0(3), vx0(3), a0(3)
      REAL chisq, chi2old, cosL0, D0, Z0
      REAL dcatr, det, free, dcatr1,dcamr1
      REAL sig_beam, tg_lenby2, tg_Zpos
      REAL lambda, lambda0, sphi
      REAL tht, fii, fii0, thetd, phid, sigma
      REAL pabs, R1,R2, chi_dif
      REAL wire_y_min,wire_y_max,sin2_ster
      real sc_hit(9,10),dcamr,dif_best, dif
      Logical ambflip, itflip, flg_dchits, flg_lowmom
      integer nhit_dc(6), nhit_sly(0:9)     ! nhit_dc(sly), nhit_sly(0..6,per region)
c
      INTEGER ind_dcgw,ncol_dcgw,WireInd
      integer nami_dcgw/0/
      REAL SCWIDTH
      integer ind_scg(6),ncol_scg,paddle,sector
      data ind_scg /0,0,0,0,0,0/
C
      DATA ifirst/1/
      DATA isec_last /10/
      DATA chi_dif/0.10/
      DATA ex  /1.,0.,0./
      DATA ez  /0.,0.,1./
C
C Trajectory migration due to multiple scattering for p = 1GeV
C      DATA ems /6*0.014, 6*0.016, 6*0.121, 6*0.139, 6*0.298, 6*0.340/
C Residual due to multiple Coulomb scattering for p=1 GeV, beta=1
C (PDG 2000,eq.23.9,23.14 for width of superlayer)
       DATA ems /6*0.003, 6*0.0031, 6*0.0054, 6*0.0063, 6*0.0077, 6*0.0087/
C
C Statement function to get correct row index in DCGW
      INTEGER MAMIND, GWind, baseind, layer, wire
      REAL MidX, MidY, MidZ, DirX, DirY, DirZ,WireHalfL,WireHalfLHV
      GWind(layer,wire) = ind_dcgw + ((layer-1)*MAX_WIRE+wire-1)*ncol_dcgw
      MidX(baseind) = RJW(baseind+DCGW_x_mid)
      MidY(baseind) = RJW(baseind+DCGW_y_mid)
      MidZ(baseind) = RJW(baseind+DCGW_z_mid)
      DirX(baseind) = RJW(baseind+DCGW_x_dir)
      DirY(baseind) = RJW(baseind+DCGW_y_dir)
      DirZ(baseind) = RJW(baseind+DCGW_z_dir)
      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
      WireHalfLHV(baseind) = RJW(baseind+DCGW_w_len_HV)
      SCwidth(paddle,sector) = RJW(ind_scg(sector)+(paddle-1)*ncol_scg+SCG_WIDTH)
C
      IF(ifirst.EQ.1) THEN
        big    = 1.E+10
        pi     = ACOS(-1.)
        piby2  = pi/2.
        raddeg = 180./pi
        ifirst = 0
      ENDIF
C
C Initialize chi2old to a big value.
      chifit  = 1000.
      chi2old = big
      IF(dpar_trk_chi2diff.GT.0.) chi_dif = dpar_trk_chi2diff

c get the index of the bosbank we need
      if (nami_dcgw.le.0) nami_dcgw = mamind(jw,'DCGW')

      ind_dcgw = jw(nami_dcgw)
      
      do while (jw(ind_dcgw-2).lt.isec.AND.jw(ind_dcgw-1).gt.0)
         ind_dcgw = jw(ind_dcgw-1)
      enddo

      if (jw(ind_dcgw-2).ne.isec) then     !Send out warning
         write(crmess,401)isec
         call recmes(crname,'W',crmess)
      else
         ncol_dcgw = JW(ind_dcgw-5)
      endif

c
C Form the uvz unit-vectors co-ordinates in the direction of the
C initial position of the track
      CALL CROSS(ez,svin(4),u0)
      CALL VUNIT(u0,u0,3)
      CALL VZERO(vx0,3)
      sphi  = ATAN2(svin(5),svin(4))

c  'define' flag for poorly detected track (to decide whether constraints are used
c     in case that ipar_trk_VXconstr<0 or ipar_trk_SCconstr<0 )

c  low momentum track (p<600MeV for full field) or very backward/forward (<15deg)
      pmin_cut=0.120*iabs(ipar_torus_current)/3860
      flg_lowmom=.FALSE.
      flg_lowmom=svin(7).lt.5*pmin_cut .or. abs(sphi).lt.0.25

c re-initialize track in case that fit diverges in hbt (start parameters not good)
      max_reinit=0
      if(level.EQ.TRK_LEVEL_HBT) then
         max_reinit=ipar_trk_maxiter/4
         if(flg_lowmom) max_reinit=max_reinit+1
      endif

      if(ipar_trk_VXconstr.NE.0 .OR. ipar_trk_SCconstr.NE.0) then 
         flg_dchits=.FALSE.
         do j=0,9,1
            nhit_sly(j)=0
         enddo
         do is=1,6
            nhit_dc(is)=0                !hits per superlayer
            do il=1,6
               if(trk(12,il+(is-1)*6,ilnk).eq.0) nhit_dc(is)=nhit_dc(is)+1
            enddo
            nhit_sly(nhit_dc(is))=nhit_sly(nhit_dc(is))+1   !#superlayers with 0..6 hits
            nhit_sly(6+(is+1)/2)=nhit_sly(6+(is+1)/2)+nhit_dc(is) !hits in region
         enddo
         nhit_sly(1)=nhit_sly(0)+nhit_sly(1)+nhit_sly(2)
c  not enough hits in DC?   <<< may be adjusted!
         flg_dchits=nhit_sly(8).lt.4 .or. nhit_sly(9).lt.5 .or. 
     & nhit_sly(0).gt.1 .or. nhit_sly(1).gt.2 .or.nhit_dc(5)*nhit_dc(6).eq.0
         if(ipar_trk_print(7).gt.1) print *,'nhit_dc,sly',nhit_dc,nhit_sly,
     &           flg_dchits,flg_lowmom
      endif
         
c in case of SC constraint: get SC hits for this sector
      mypar_SCconstr=0
      if(ipar_trk_SCconstr.NE.0) then
         if(ind_scg(1).eq.0) then
           do k=1,6
             ind_scg(k)=MLINK(jw,'SCG ',k)
             if(ind_scg(k).eq.0) then
                if(k.gt.1) ind_scg(k)=ind_scg(1)
                if(k.eq.1) ind_scg(1)=-1
             else
               ncol_scg=jw(ind_scg(k)-5)
             endif
           enddo
         endif
         if(ilnk.eq.1 .or. isec.ne.isec_last) then
            isec_last=isec
            nhit_sc=0
            ind_scr=MLINK(iw,'SCR ',isec)
            if(ind_scr.gt.0) then
               ncol_sc=iw(ind_scr-5)
               nd_sc=iw(ind_scr)-1
               if(nd_sc.gt.0) then
                 do j=0,nd_sc,ncol_sc
                   if(IW(ind_scr+j+10).eq.15.and.nhit_sc.lt.10) then    !status ok
                     nhit_sc=nhit_sc+1
                     ipl=IW(ind_scr+j+1)
c  hits in SCR are stored in sector coord.
                     call sda_rotdsec(RW(ind_scr+j+4),sc_hit(1,nhit_sc),1)
                     sc_hit(7,nhit_sc)=ipl
                     sc_hit(8,nhit_sc)=sc_hit(6,nhit_sc)*SNGL(dpar_trk_SCerr_scale)
                     if(ind_scg(isec).gt.0) then
                        sc_hit(9,nhit_sc)=SCwidth(ipl,isec)/2.0
                     else
                        sc_hit(9,nhit_sc)=7.5
                     endif
                     if(dpar_trk_SCerr_scale.LE.1.0) 
     &                        sc_hit(9,nhit_sc)=sc_hit(9,nhit_sc)/10.
                   endif
                 enddo
               endif
            endif
            if(ipar_trk_print(7).gt.1) then
               if(nhit_sc.gt.0) then
                  do j=1,nhit_sc
          print *,'SC sect',isec,sc_hit(7,j),(sc_hit(k,j),k=1,3),(sc_hit(k,j),k=8,9)
                  enddo
               else
                  print *,'no SC hit in sect',isec
               endif
            endif
         endif
         if(nhit_sc.gt.0) then
           my_scplane=0
           if(ipar_trk_SCconstr.GT.0) then
             mypar_SCconstr=ipar_trk_SCconstr
           else if(flg_dchits .or. flg_lowmom) then
              mypar_SCconstr=-ipar_trk_SCconstr
           endif
        endif
      endif

C check whether we should start anywhere else than beamline ('vertex')
      ilmax=36
      if(ipar_trk_fitregion.GT.0) then
         if(BTEST(ipar_trk_fitregion,0)) ilmax=12
         if(BTEST(ipar_trk_fitregion,1)) ilmax=24
         if(BTEST(ipar_trk_fitregion,2)) ilmax=36
         if(BTEST(ipar_trk_fitregion,3)) then
            ilmax=30
            do il=19,24
               trk(12,il,ilnk)=1
            enddo
         endif
         if(ipmin.ge.4) then
            ilmin=ipmin-3
            iwr = ABS(trk(1,ilmin,ilnk))
            WireInd = GWind(ilmin,iwr)
            vx0(1) = MidZ(WireInd)
            vx0(2) = MidX(WireInd)
            vx0(3) = MidY(WireInd)
c  mark hits that are skipped when calculating chi2
            do il=1,ilmin-1,1
               trk(12,il,ilnk)=1
            enddo
            do il=ilmax+1,36,1
               trk(12,il,ilnk)=1
            enddo
         endif
      endif

c in case of 'vertex' constraint:
c if 'vertices' outside ranges given by dpar_SigmaBeam or dpar_TargetLen
c force start value to be at dpar_SigmaBeam,dpar_targetLen
c NOTE: SigmaBeam, TargetLen have to chosen correspondingly wide
c
      mypar_vxconstr = 0
      if(ipmin.EQ.1 .AND. ipar_trk_vxconstr.NE.0) then
        if(flg_dchits .or. flg_lowmom .or. ipar_trk_vxconstr.gt.0) then 
c     &         (Level.EQ.4 .AND. ipar_trk_vxconstr.LT.0)) then
           mypar_vxconstr = IABS(ipar_trk_vxconstr)
c
c  ipar_trk_vxconstr<0: only 'vertex' constraint 
c                       for low momenta tracks or if too few hits in DC
           sig_beam =SNGL(dpar_SigmaBeam)
           tg_lenby2=SNGL(dpar_TargetLen)/2.
           tg_Zpos  =SNGL(dpar_TargetPos(3))
           if(mypar_vxconstr.EQ.1 .OR. mypar_vxconstr.EQ.3) then
              if( ABS(svin(2)).GT.2.0*sig_beam) svin(2)=2.0*SIGN(sig_beam,svin(2))
              if( ABS(svin(3)).GT.2.0*sig_beam) svin(3)=2.0*SIGN(sig_beam,svin(3))
           endif
           if(mypar_vxconstr.GT.1 .AND. ABS(svin(1)-tg_Zpos).GT.tg_lenby2) 
     &                 svin(1)=tg_Zpos+SIGN(tg_lenby2,svin(1))
        endif
      endif
      if(ipar_trk_print(7).gt.1) then
        if(mypar_vxconstr.ne.0 .or. mypar_scconstr.ne.0) then
           print *,'ANA_TRFIT: use vertex / SC constraints',mypar_vxconstr,mypar_scconstr
        endif
        if(ipar_trk_print(7).gt.1) then
           print '(A,3I3,3F6.1,5F6.3,F4.0)',' sec,lev,it=',isec,level,ilnk,svin
        endif
      endif

      diff0(1) = svin(1) - vx0(1)
      diff0(2) = svin(2) - vx0(2)
      diff0(3) = svin(3) - vx0(3)
      D0 = VDOT(diff0,u0,3)
      Z0 = svin(3)
      do j=1,9
        svin0(j)=svin(j)
      enddo
C Errors due to Multiple Scattering
      if(ipar_trk_multscat.gt.0) then
         inv_beta2=0
         if(level.GE.TRK_LEVEL_TBT) then
            if(svin(8).lt.0.3) then
               inv_beta2=1./0.3**3
            else if(svin(8).gt.1.) then
               inv_beta2=1.
            else
               inv_beta2=1./svin(8)/svin(8)
            endif
         endif
      endif
      DO il = 1,npl_dc
         is = (il-1)/6 + 1
         wwd(il)=trk(5,il,ilnk)**2 + dc_Sigma_doca(is)**2 
         if(level.eq.2 .and.flg_lowmom) wwd(il)=wwd(il)*2
         if(ipar_trk_multscat.gt.0) wwd(il)=wwd(il)+inv_beta2*(ems(il)/svin(7))**2
      ENDDO
C
C Track Fitting iterations
C

c lcs - These are too few iterations for some tracks
c      maxiter = max(4,min(ipar_trk_maxiter,int(3.0+1.0/svin(7))))

      maxiter = ipar_trk_maxiter
      DO 200 iter=1,maxiter
	ambflip = .FALSE.
C
c        IF(Level.EQ.TRK_LEVEL_tbt .AND. iter.GT.ipar_trk_calcTM) THEN
C
C Trajectory propagation
C--------CALL sda_swim(isec,ipl_min,ipl_max,if_mat, svin)
c          CALL sda_swim(isec,ipmin, nsc_max,    0, svin)
c        ELSE
          CALL sda_swim(isec,ipmin, nsc_max,    1, svin)
c        ENDIF

C
C Calculation of derivatives (dtrsp) and dcat
        lambda0= ASIN(svin(6))
        cosL0  = COS(lambda0)
        fii0   = ATAN2(svin(5),svin(4))
        ilay   = 0
        chisq  = 0.
C
C Vertex constraint due to transverse beam size (electron beam)
C
        IF(mypar_vxconstr.EQ.1 .OR. mypar_vxconstr.EQ.3) THEN
          ilay = ilay + 1
          CALL CROSS(ex,sda_trp(4,1), u)
          CALL VUNIT(u,u,3)
C Assume here uniform magnetic field along z-axis (ipar_trk_calcTM=0)
          IF(ipar_trk_calcTM.EQ.0) THEN
            diff(1) = sda_trp(1,1) - svin(1)
            diff(2) = sda_trp(2,1) - svin(2)
            diff(3) = sda_trp(3,1) - svin(3)
            dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            dtrsp(2,ilay) = sda_trp(7,1)*u(3)/cosL0
            CALL CROSS(diff,u, temp)
            dtrsp(3,ilay) = temp(3)
            dtrsp(4,ilay) = VDOT(u0,u,3)
            dtrsp(5,ilay) = u(3)
C Inhomogeneous magnetic field (ipar_trk_calcTM>0)
          ELSE
            dtrsp(1,ilay) = sda_tmat(4,1,1)
            dtrsp(2,ilay) = sda_tmat(5,2,1)*u(3)
            dtrsp(3,ilay) = sda_tmat(4,3,1)
            dtrsp(4,ilay) = sda_tmat(4,4,1)
            dtrsp(5,ilay) = sda_tmat(5,5,1)*u(3)
C No magnetic field but transport matrix was computed
            IF(ipar_trk_magtyp.EQ.5) THEN
              diff(1) = sda_trp(1,1) - svin(1)
              diff(2) = sda_trp(2,1) - svin(2)
              diff(3) = sda_trp(3,1) - svin(3)
              dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            ENDIF
          ENDIF
C Calculation of vector VX(ilay) = Dmeas - Dtrial and  chi2
          dcatr = VDOT(sda_trp(1,1),u,3)
          vx(ilay) = SNGL(dpar_BeamOff(2)) - dcatr
C Chi-square accumulated with weight correction
          sigma = sig_beam
          IF(sigma.LT.0.005) sigma = 0.02
          weight(ilay) = 1./sigma**2
          chisq = chisq + vx(ilay)*vx(ilay)*weight(ilay)
        ENDIF
C
C Vertex constraint due to target length along the beam (very thin targets)
C
        IF(mypar_vxconstr.GT.1) then
          ilay = ilay + 1
          CALL CROSS(ez,sda_trp(4,1), u)
          CALL VUNIT(u,u,3)
C Assume here uniform magnetic field along z-axis 
          IF(ipar_trk_calcTM.EQ.0) THEN
            diff(1) = sda_trp(1,1) - svin(1)
            diff(2) = sda_trp(2,1) - svin(2)
            diff(3) = sda_trp(3,1) - svin(3)
            dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            dtrsp(2,ilay) = sda_trp(7,1)*u(3)/cosL0
            CALL CROSS(diff,u, temp)
            dtrsp(3,ilay) = temp(3)
            dtrsp(4,ilay) = VDOT(u0,u,3)
            dtrsp(5,ilay) = u(3)
C Inhomogeneous magnetic field (ipar_trk_calcTM>0)
          ELSE
            dtrsp(1,ilay) = sda_tmat(4,1,1)
            dtrsp(2,ilay) = sda_tmat(5,2,1)*u(3)
            dtrsp(3,ilay) = sda_tmat(4,3,1)
            dtrsp(4,ilay) = sda_tmat(4,4,1)
            dtrsp(5,ilay) = sda_tmat(5,5,1)*u(3)
C No magnetic field but transport matrix was computed
            IF(ipar_trk_magtyp.EQ.5) THEN
              diff(1) = sda_trp(1,1) - svin(1)
              diff(2) = sda_trp(2,1) - svin(2)
              diff(3) = sda_trp(3,1) - svin(3)
              dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            ENDIF
          ENDIF
C Calculation of vector VX(ilay) = Dmeas - Dtrial and  chi2
          dcatr = VDOT(sda_trp(1,1),u,3)
          vx(ilay) = -tg_Zpos * SIN(sphi) - dcatr
C Chi-square accumulated with weight correction
          sigma = tg_lenby2
          IF(sigma.LT.0.005) sigma = 0.50
          sigma = sigma*SIN(sphi)/1.732           ! Flat distr. along target 
          weight(ilay) = 1./sigma**2
          chisq = chisq + vx(ilay)*vx(ilay)*weight(ilay)
        ENDIF
C
C Now loop over all layers of DC
C
        DO 30 il = 1,npl_dc
          ipl=il+nst_max			!detector-plane in 'sdaswim'
          IF(trk(12,il,ilnk).GT.0.) GO TO 30    ! Discard this hit
          is = (il-1)/6 + 1
          iwr = trk(1,il,ilnk)
          ilay = ilay + 1
          WireInd = GWind(il,iwr)
          temp(1) = DirZ(WireInd)
          temp(2) = DirX(WireInd)
          temp(3) = DirY(WireInd)
          CALL CROSS(temp(1),sda_trp(4,ipl), u)
          CALL VUNIT(u,u,3)
C Assume here uniform magnetic field along z-axis 
          IF(ipar_trk_calcTM.EQ.0) THEN
            diff(1) = sda_trp(1,ipl) - svin(1)
            diff(2) = sda_trp(2,ipl) - svin(2)
            diff(3) = sda_trp(3,ipl) - svin(3)
            dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            dtrsp(2,ilay) = sda_trp(7,ipl)*u(3)/cosL0
            CALL CROSS(diff,u, temp)
            dtrsp(3,ilay) = temp(3)
            dtrsp(4,ilay) = VDOT(u0,u,3)
            dtrsp(5,ilay) = u(3)
C Inhomogeneous magnetic field (ipar_trk_calcTM>0)
          ELSE
            dtrsp(1,ilay) = sda_tmat(4,1,ipl)
            dtrsp(2,ilay) = sda_tmat(5,2,ipl)*u(3)
            dtrsp(3,ilay) = sda_tmat(4,3,ipl)
            dtrsp(4,ilay) = sda_tmat(4,4,ipl)
            dtrsp(5,ilay) = sda_tmat(5,5,ipl)*u(3)
C No magnetic field but transport matrix was computed
            IF(ipar_trk_magtyp.EQ.5) THEN
              diff(1) = sda_trp(1,ipl) - svin(1)
              diff(2) = sda_trp(2,ipl) - svin(2)
              diff(3) = sda_trp(3,ipl) - svin(3)
              dtrsp(1,ilay) = -svin(7)*VDOT(diff,u,3)
            ENDIF
          ENDIF
C Calculation of vector VX(ilay) = Dmeas - Dtrial and  chi2
          diff(1) = sda_trp(1,ipl) - MidZ(WireInd)
          diff(2) = sda_trp(2,ipl) - MidX(WireInd)
          diff(3) = sda_trp(3,ipl) - MidY(WireInd)

          trk(7,il,ilnk) = VDOT(diff,u,3)                         ! Dcat
C We got amb wrong in lramb
c          vx(ilay)  = trk(4,il,ilnk) - trk(7,il,ilnk)             ! Dcam - Dcat
c          print *, 'ana_trfit',il,trk(4,il,ilnk),trk(7,il,ilnk),vx(ilay),ambflip,itflip
c          itflip = .false.
          IF(level.GE.TRK_LEVEL_tbt)then
             if(trk(4,il,ilnk)*trk(7,il,ilnk).LT.0.0)then
                if(abs(trk(4,il,ilnk)).le.2.0*trk(5,il,ilnk))then
                   trk(4,il,ilnk)=-1.0*trk(4,il,ilnk)
                   ambflip=.TRUE.
c                   itflip=.TRUE.
                endif
             endif
          endif
C
          vx(ilay)  = trk(4,il,ilnk) - trk(7,il,ilnk)             ! Dcam - Dcat
C Chi-square accumulated with weight correction
          weight(ilay) = 1/wwd(il)
          if(level.eq.2 .and. iter.lt.4) weight(ilay)=weight(ilay)/float(7-iter*2)
          if(ipar_trk_print(7).gt.2) 
     & print *,il,iwr,weight(ilay),vx(ilay),diff,(sda_trp(i,ipl),i=1,6)
          chisq = chisq + weight(ilay)*vx(ilay)*vx(ilay)
   30   CONTINUE
C
C SC constraint 
C
        if(mypar_SCconstr.EQ.0) goto 35
c  no transport matrix computed for this iteration? 
        if(ipar_trk_calcTM.EQ.0) goto 35
c  track oriented in other direction?
        if(my_scplane.gt.0 .AND. sda_trp(1,my_scplane).GT.900) my_scplane=0
c        if((sc_hit(1,my_schit)-sda_trp(1,my_scplane))**2+
c     &      (sc_hit(2,my_schit)-sda_trp(2,my_scplane))**2.GT.50**2) my_scplane=0 
        if(my_scplane.le.0) then
           dif_best=60.**2           !max. cut in SC width
           do k=nsc_min,nsc_max
              if(sda_trp(1,k).lt.900) then
                 do j=1,nhit_sc
                    dif=(sc_hit(1,j)-sda_trp(1,k))**2+(sc_hit(2,j)-sda_trp(2,k))**2
                    if(dif.lt.dif_best) then
                       dif_best=dif
                       my_schit=j
                       my_scplane=k
                    endif
                 enddo
              endif
           enddo
           if(my_scplane.le.0) then
             if(ipar_trk_print(7).gt.2) then
                do k=nsc_min,nsc_max
                   if(sda_trp(1,k).lt.900) then
                      print *,'sc_plane (no match)',k,(sda_trp(j,k),j=1,3)
                   endif
                enddo
             endif
             if(flg_lowmom .AND. iter.LT.3) my_scplane=-1    !start value off? 
             goto 35
           endif
           if(ipar_trk_print(7).gt.2)
     & print *,'sc plane',my_scplane,(sda_trp(j,my_scplane),j=1,3),dif_best
        endif
C
C SC paddle constraint (position along paddle)
C
        if(mypar_SCconstr.EQ.1 .OR. mypar_SCconstr.EQ.3) then
          ilay = ilay + 1
          CALL CROSS(ex,sda_trp(4,my_scplane), u)
          CALL VUNIT(u,u,3)
          dtrsp(1,ilay) = sda_tmat(4,1,my_scplane)
          dtrsp(2,ilay) = sda_tmat(5,2,my_scplane)*u(3)
          dtrsp(3,ilay) = sda_tmat(4,3,my_scplane)
          dtrsp(4,ilay) = sda_tmat(4,4,my_scplane)
          dtrsp(5,ilay) = sda_tmat(5,5,my_scplane)*u(3)
C Calculation of vector VX(ilay) = Dmeas - Dtrial 
          dcatr = VDOT(sda_trp(1,my_scplane),u,3)
          dcamr = VDOT(sc_hit(1,my_schit),u,3)
          vx(ilay) = dcamr - dcatr
C Chi-square accumulated with weight correction
          sigma = sc_hit(8,my_schit)
          IF(sigma.LT.0.5) sigma = 2.5
          weight(ilay) = 1./sigma**2
          chisq = chisq + vx(ilay)*vx(ilay)*weight(ilay)
        ENDIF
C
C SC paddle constraint perpendicular to paddle (=paddle width)
C
        IF(mypar_SCconstr.GT.1) then
          ilay = ilay + 1
          CALL CROSS(ez,sda_trp(4,my_scplane), u)
          CALL VUNIT(u,u,3)
          dtrsp(1,ilay) = sda_tmat(4,1,my_scplane)
          dtrsp(2,ilay) = sda_tmat(5,2,my_scplane)*u(3)
          dtrsp(3,ilay) = sda_tmat(4,3,my_scplane)
          dtrsp(4,ilay) = sda_tmat(4,4,my_scplane)
          dtrsp(5,ilay) = sda_tmat(5,5,my_scplane)*u(3)
C Calculation of vector VX(ilay) = Dmeas - Dtrial 
          dcatr1 = VDOT(sda_trp(1,my_scplane),u,3)
          dcamr1 = VDOT(sc_hit(1,my_schit),u,3)
          vx(ilay) = dcamr1 - dcatr1
C Chi-square accumulated with weight correction
          sigma = sc_hit(9,my_schit)
          weight(ilay) = 1./sigma**2
          chisq = chisq + vx(ilay)*vx(ilay)*weight(ilay)
        ENDIF
C
C Normalize chi-square by the no. of degree of freedom
 35     continue
	if(ilay.LT.6) goto 201
        free = FLOAT(ilay) - 5.
        if(chisq.GT.0.0005) chifit = chisq/free

        iterl = iter
C
C Calculation of matrix alfa(5,5)
        DO j=1,5
          DO k=1,5
            alfa(j,k) = 0.
c            alfaw(j,k) = 0.
            DO il=1,ilay
              R1 = dtrsp(j,il)*dtrsp(k,il)
              alfa(j,k) = alfa(j,k) + R1*weight(il)    ! proper chi2 fit
c              IF(ipar_trk_multscat.GT.0) THEN
c                 alfaw(j,k) = alfaw(j,k) + R1/wwd(il)
c              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
C for no magnetic field, p must be an independent variable in the fit.
C modify the alfa matrix appropriately:
        if(ipar_trk_magtyp.eq.5) then
           alfa(1,1) = 1.
           do j=2,5
              alfa(1,j) = 0.
              alfa(j,1) = 0.
           enddo
        endif

C Want to do proper Chi2 fit: need to put the weights into beta as well
C the call to ana_mvprod is the last time this vx is used.
        DO il=1,ilay
           vx(il) = vx(il)*weight(il)
        ENDDO
C
C Calculation of vector BETA(5)
        CALL ana_mvprod(dtrsp,vx,beta,5,ilay)
C
C Invertion of matrix ALFA(5,5)
        CALL ana_mxinv(alfa,5,det)
        IF (det.EQ.0.) THEN
           if(max_reinit.gt.0) goto 190
           GO TO 201
        ENDIF
C
C Calculation of corrections DX(5)
        CALL ana_mvprod(alfa,beta,dx,5,5)

C
C Get new {p,Theta,Phi,d0,z0} and svin from previous svin and
C corrections DX
        IF (ipar_trk_magtyp.EQ.5) THEN    ! no B field, so
           pabs = svin(7)                 ! no sensitivity to p
        ELSE
           pabs = 1./( 1./svin(7) + dx(1) )
        ENDIF
c
c Since this is fitting |1/p| and q separately, instead of q*|1/p| like 
c any sensible tracking program, correct for when the fit makes |1/p| < 0,
c by flipping charge.
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^you could also take q/p instead  (fk)
c
        IF (pabs.lt.0.03) THEN        !bending opposite way from before
           pabs= abs(pabs)            !positive momentum
           if(pabs.lt.0.03) pabs=0.03 !that approaches 1/0
           if(iter.gt.3) then
              svin(9) = -svin(9) !change charge of track
              if(ipar_trk_print(7).gt.1) 
     &            print *,' iter',iter,' changed charge for mom=',pabs
           endif
        ENDIF
        lambda = lambda0 + dx(2)
        tht = piby2 - lambda
        fii = fii0 + dx(3)
C New DCA & Z0 at origin
        D0 = D0 + dx(4)
        Z0 = Z0 + dx(5)
C New starting values for the next tracing
        svin(4) = SIN(tht)*COS(fii)
        svin(5) = SIN(tht)*SIN(fii)
        svin(6) = COS(tht)
        if(ipmin.EQ.1) then
          svin(1) = -D0/SIN(fii)
          svin(2) =  svin(2)
          svin(3) =  Z0
        else
          CALL CROSS(ez,svin(4),a0)
          CALL VUNIT(a0,a0,3)
          svin(1) = vx0(1) + D0*a0(1) - 3.*svin(4)
          svin(2) = vx0(2) + D0*a0(2) - 3.*svin(5)
          svin(3) = Z0
        endif
C
        if(ipar_trk_print(7).gt.1) 
     & print '(A,I2,I3,F10.1,3F6.1,5F6.3,F4.0)',' iter=',iter,my_scplane,chisq,svin

        if(max_reinit.gt.0 .and.
     &  (pabs.lt.pmin_cut .or. abs(pabs-svin(7)).gt.0.8*abs(svin(7)))) goto 190

        svin(7) = pabs

        IF(ABS(chi2old-chisq).GT.chi_dif.OR.ambflip .OR. max_reinit.gt.0) then
           chi2old = chisq
           goto 200
        else
           GO TO 201            ! early exit
        endif
c
c for failed interations in level 2: re-initialize track parameters
 190    continue
        max_reinit=max_reinit-1
        if(ipar_trk_print(7).gt.1) print *,' reinit',max_reinit,pabs,svin
        if(my_scplane.lt.0) then
           my_scplane=0
           svin(7)=svin0(7)*1.1
        elseif(pabs.lt.pmin_cut)  then
           svin(7)=svin0(7)*1.1
           if(max_reinit.eq.0) svin(7)=svin0(7)*0.9
        else
           if(svin(7).le.svin0(7)) then
              svin(7)=svin0(7)*1.1
           else
              svin(7)=svin0(7)*0.9
           endif
        endif
        do j=1,6
           svin(j)=svin0(j)
        enddo
        svin(9)=svin0(9)
        D0 = VDOT(diff0,u0,3)    
        Z0 = svin(3)
        chi2old=9000.
  200 CONTINUE                    ! End of loop over iterations
C
  201 CONTINUE

C Print the result of last iteration
      IF(ipar_trk_print(7).GE.1) THEN
        iter = iterl - 1
        thetd = tht*raddeg
        phid  = fii*raddeg
        WRITE(6,301) Level,isec,ilnk, iter,chifit
        WRITE(6,302) DX, pabs,thetd,phid,D0,Z0,(svin(i),i=1,3)
      ENDIF

C Check whether 
C Check if make sense to perform the global swimming
      if(chifit.GT.2*dpar_trk_tbtfit_chi2 .OR. pabs.GT.20.) goto 999

c  Store the fit parameter {q/p,lambda,fii,D0,Z0} and Covariance matrix
      trk_err(1,ilnk)=svin(9)/pabs
      trk_err(2,ilnk)=lambda
      trk_err(3,ilnk)=fii
      trk_err(4,ilnk)=D0
      trk_err(5,ilnk)=Z0
      id=5
      do j=1,5
        do k=j,5
          id=id+1
          trk_err(id,ilnk)=alfa(j,k)
c     need to correct covariance matrix for q/p instead of just 1/p
          if(j.eq.1.and.k.gt.1) then 
             trk_err(id,ilnk)=trk_err(id,ilnk)*svin(9)
          endif
        enddo
      enddo

C Evaluate uncertainties for parameters

c      IF(ipar_trk_multscat.GT.0 .AND. Level.EQ.4) THEN
c        CALL ana_mxinv(alfaw,5,det)
        DO j = 1,5
c          errx(j) = SQRT(alfa(j,j))
          errx(j) = sign(SQRT(abs(alfa(j,j))),alfa(j,j))
        ENDDO
        IF(ipar_trk_print(7).GE.1) THEN
          R1 = errx(2)*raddeg
          R2 = errx(3)*raddeg
          WRITE(6,304) errx(1),R1,R2,errx(4),errx(5)
        ENDIF
c      ENDIF

C
C Calculation of final trajectory and chi-square
C

C-----CALL sda_swim(isec,ipl_min,ipl_max,if_mat, svin)
      CALL sda_swim(isec,  ipmin,   npln,     0, svin)
C
      ilay   = 0
      chisq = 0.
      chifit = 1000.
      DO 240 il = 1,npl_dc
        ipl=il+nst_max				!detector plane in 'sdaswim'
        IF(trk(12,il,ilnk).GT.0.) GO TO 240    !This hit was not used in fit
        is = (il-1)/6 + 1
        iwr = abs(trk(1,il,ilnk))
        IF (iwr.eq.0) GO TO 240                  ! Discard this hit
        WireInd = GWind(il,iwr)

        temp(1) = DirZ(WireInd)
        temp(2) = DirX(WireInd)
        temp(3) = DirY(WireInd)

        CALL CROSS(temp(1),sda_trp(4,ipl), u)
        CALL VUNIT(u,u,3)
C Calculation of vector vx(ilay) = Dcam - Dcat  and  chi2
        diff(1) = sda_trp(1,ipl) - MidZ(WireInd)
        diff(2) = sda_trp(2,ipl) - MidX(WireInd)
        diff(3) = sda_trp(3,ipl) - MidY(WireInd)
        trk(7,il,ilnk) = VDOT(diff,u,3)                        ! Dcat

        ilay = ilay + 1
        vx(ilay) = trk(4,il,ilnk) - trk(7,il,ilnk)              ! Dcam - Dcat
C Chi-square accumulated with weight correction
        weight(ilay) = 1./wwd(il)
        chisq = chisq + weight(ilay)*vx(ilay)*vx(ilay)
C
C add to chi-square if final track is outside the sector.
C Rob F., please check to see if I have this correct.
C We scale up the sigma on the hit by 1/sin_stereo, where sin_stereo is the
C stereo angle of a wire in the stereo SL of this region to get some guess
C as to an uncertainty in how far the track is outside the sector.
C
	IF(il.LE.12)THEN
	  wire_y_min = -1.0*abs(WireHalfL(WireInd)*temp(3))
	  wire_y_max = abs(WireHalfLHV(WireInd)*temp(3))
	  sin2_ster = 1.0 - DirY(GWind(3,iwr))**2
	ELSEIF(il.le.24)THEN
	  wire_y_max = abs(WireHalfL(WireInd)*temp(3))
	  wire_y_min = -1.0*abs(WireHalfLHV(WireInd)*temp(3))
	  sin2_ster = 1.0 - DirY(GWind(21,iwr))**2
	ELSE
	  wire_y_max = abs(WireHalfL(WireInd)*temp(3))
	  wire_y_min = -1.0*abs(WireHalfLHV(WireInd)*temp(3))
	  sin2_ster = 1.0 - DirY(GWind(33,iwr))**2
	ENDIF
C
	IF(sda_trp(3,ipl).GT.wire_y_max)THEN
	  chisq = chisq + (sda_trp(3,ipl)-wire_y_max)**2*weight(ilay)*sin2_ster
	ELSEIF(sda_trp(3,ipl).LT.wire_y_min)THEN
	  chisq = chisq + (sda_trp(3,ipl)-wire_y_min)**2*weight(ilay)*sin2_ster
	ENDIF
C
  240 CONTINUE
C
      if (ilay.GT.5) then
       free = FLOAT(ilay) - 5.
       if(chisq.GT.0.0005)chifit = chisq/free
      endif

      IF(ipar_trk_print(7).GE.1) WRITE(6,303) iterl,chifit,int(free)

C
  301 FORMAT(/' LEVEL =',I2,'  isec/itrk/iter =',3I3,'  Chi2 =',E12.6)
  302 FORMAT(' Cor: 1/P,L0,PH=',F8.5,2F10.5,'  D0,Z0=',F8.4,8X,F8.
     1      4/
     2      ' New:  P ,TH,PH=',F8.5,2F10.5,'  D0,Z0=',F8.4,8X,F8.4/
     3      13X,'                                Vertex=',3F8.4)
  303 FORMAT(' Iter=',I2,'   Chi2 =',E12.6, '   DOF = ',I2)
  304 FORMAT(' Errx: P ,TH,PH=',F8.5,2F10.5,'  D0,Z0=',F8.4,8X,F8.4)
  401 format('Error getting DCGW bank # ',i1,'.')
C
  999 CONTINUE
      RETURN
      END


