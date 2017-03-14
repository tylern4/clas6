C======================================================================
      SUBROUTINE ana_lramb(isec,is,digi,psi,wgtslp, chib,slopD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resolves Left-Right AMBiguities for track
C-                         segment in a SuperLayer
C-
C-   Input:    isec       - sector number
C-             is         - superlayer number
C-             digi       - hit info {iw,tdc,Td,-/+Dm,SigDm} for a SL
C-             psi        - projection track angle w.r. to R (or normal to SL)
C-             wgtslp     - weight for slope difference from tan(psi)
C-
C-   Outputs : chib       - best chi2 from all 4 solutions.
C-             slopD      - slope difference between track segment & PR fit
C-
C-   Controls:
C-
C-
C-   Library belongs    : libana.a
C-
C-   Calls              : none
C-
C-   Created   26-OCT-1990   Bogdan Niczyporuk
C-   Modified  29-JULY-1992  Billy Leung
C-   Modified  23-FEB-1996   Bogdan Niczyporuk
C-
C-   Called by trk_dcam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      INTEGER nlay
      PARAMETER (nlay = 6)
*
*  I/O variables
*  -------------
      INTEGER isec, is
      REAL digi(5,nlay), psi, chib, slopD, wgtslp 
*
*  Include Files
*  -------------
#include "trktcl_var.inc"
#include "sdaswim.inc"
#include "clas_offsets.inc"
#include "wcs.inc"
#include "bcs.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_lramb.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.10 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/02 18:18:05 $')
      PARAMETER ( CAUTHO = '$Author: claschef $')
      DATA CRCSID/
     1'$Id: ana_lramb.F,v 1.10 1999/03/02 18:18:05 claschef Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_LRAMB')
      PARAMETER (CRAUTH='Franz Klein')

C
C Local variables
      INTEGER i, il,il3, istart, J1,J2,IL1, J1old(nlay)
      INTEGER max_miss, nmiss, ilmin
      INTEGER LLL,LLR,LRL,LRR, LRB
C
      REAL xr(nlay), phw(nlay),dlw(nlay), wstg(nlay), w, Scut 
      REAL dw1LL, dw2LL, dw1LR, dw2LR, dw1RL, dw2RL, dw1RR, dw2RR
      REAL fw0, fw1, fw2, fdet
      REAL chiLL, chiLR, chiRL, chiRR
      REAL dat0LL, dat0LR, dat0RL, dat0RR
      REAL drawLL, drawLR, drawRL, drawRR
      REAL dsegLL, dsegLR, dsegRL, dsegRR
      REAL predLL, predLR, predRL, predRR
      REAL sgnLL, sgnLR, sgnRL, sgnRR
      REAL slopLL, slopLR, slopRL, slopRR, slopTF, slopB
      REAL cpsi, df, deti, ddist(nlay), derr(nlay), deltaR 
C
      LOGICAL miss(nlay)       ! inefficiency of any cause
C
      REAL xmin_nlay
      PARAMETER (xmin_nlay = 3.) ! min. number of layers needed

      INTEGER NLAYinSL(6)        
      DATA NLAYinSL / 4, 6, 6, 6, 6, 6/

c For BOS bank manipulation
      INTEGER ind_dcgm, ncol_dcgm, ipt_gm, nami_dcgm
      data nami_dcgm/0/
      integer MAMIND, GMind, layer
C Functions to get index of appropriate row in DCGM and DCGW
      GMind(layer) = ind_dcgm + (layer-1)*ncol_dcgm
      
C
C
C LR_AMB begins
C
C Maximum inefficiency (in terms of #layer) possible.
      max_miss =  nlay - IFIX(xmin_nlay)
C Initialize chi-square best and second best to huge values.
      chib = 1000.
C When "istart" set to non-zero value will skip re-doing with
C different starting layers (see last IF block) even if bad chi2 results.
C
      istart = 1                              ! istart can be set to 2
C
      cpsi = COS(psi)
      slopTF = TAN(psi)
      Scut = SNGL(dpar_trk_lrambfit_chi2)*SQRT( FLOAT(nlay)/FLOAT(NLAYinSL(is)) )

c get DCGM index I need
      if (nami_dcgm.le.0) nami_dcgm = mamind(jw,'DCGM')

      ind_dcgm = jw(nami_dcgm)
      
      do while (jw(ind_dcgm-2).lt.isec.AND.jw(ind_dcgm-1).gt.0)
         ind_dcgm = jw(ind_dcgm-1)
      enddo

      if (jw(ind_dcgm-2).ne.isec) then     !Send out warning
         write(crmess,401)isec
         call recmes(crname,'W',crmess)
      else
         ncol_dcgm = JW(ind_dcgm-5)
      endif

C
C Local coordinate system where "xr" is X-axis along R of SL (or normal to SL)
C and "wstg", Y-axis along the layer (arc).

      ilmin = (is-1)*nlay
      il3 = ilmin + 3
      deltaR = sda_pln(7,il3+nst_max,isec) - sda_pln(7,il3+nst_max-1,isec)
      DO i = 1,nlay
        il = ilmin + i
        w = ABS(digi(1,i))
        ipt_gm = GMind(il)
        IF(w.GT.JW(ipt_gm+DCGM_max_cylw)) THEN    ! on the plane
           phw(i) = RJW(ipt_gm+DCGM_theta_start) + JW(ipt_gm+DCGM_max_cylw)
     &          * RJW(ipt_gm+DCGM_d_theta)        ! theta of last cyl wire
           dlw(i) = (w-JW(ipt_gm+DCGM_max_cylw))*RJW(ipt_gm+DCGM_p_sep)
     &          *RJW(ipt_gm+DCGM_y_nmid)          ! correct for rotated chamber
        ELSE                                      ! on cylinder
           phw(i) = RJW(ipt_gm+DCGM_theta_start) + 
     &          (w-1.)*RJW(ipt_gm+DCGM_d_theta)
           dlw(i) = 0.     
        ENDIF
        xr(i)   = FLOAT(i - 1)*deltaR
        wstg(i) = (phw(i) - phw(1))*sda_pln(7,il+nst_max,isec) + dlw(i) - dlw(1)
        ddist(i)= ABS(digi(4,i))/cpsi
        derr(i) = digi(5,i)/cpsi
      ENDDO
C
C Remember the old starting layers, initially none.
      DO i = 1,nlay
        miss(i) = .FALSE.
        J1old(i) = 0
      ENDDO
C
   30 CONTINUE
C
C Find first 2 starting layers, total the no. of inefficient layers
C if no. of required layers met, J1 will be the first; J2, the second.
      nmiss = 0
      J1 = 0
      J2 = 0
      DO il = 1,nlay
        IF (digi(1,il).LE.0.) THEN   ! -ve no. mean inefficiency
          nmiss = nmiss + 1
          miss(il) = .TRUE.
        ELSE IF (J1old(il).NE.1) THEN
          IF (J1.EQ.0) THEN          ! Test if J1 is picked
            J1 = il
          ELSE
            IF (J2.EQ.0) THEN        ! Test if J2 is picked
              J2 = il
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C Too many inefficient layers, leave subroutine
      IF ((nmiss.GT.max_miss) .OR. (J1.EQ.0) .OR. (J2.EQ.0)) THEN
        GO TO 999
      ENDIF
C
C Initialize four starting solutions (first 2 layers)
      fw0 = 2.
      fw1 = xr(J1) + xr(J2)               ! Sum of the x s
      fw2 = xr(J1)*xr(J1) + xr(J2)*xr(J2) ! Sum of the x squares
C     
      dw1LL = -ddist(J1)+wstg(J1)           -ddist(J2)+wstg(J2)
      dw2LL =(-ddist(J1)+wstg(J1))*xr(J1) +(-ddist(J2)+wstg(J2))*xr(J2)
      dw1LR = -ddist(J1)+wstg(J1)           +ddist(J2)+wstg(J2)
      dw2LR =(-ddist(J1)+wstg(J1))*xr(J1) +( ddist(J2)+wstg(J2))*xr(J2)
      dw1RL =  ddist(J1)+wstg(J1)           -ddist(J2)+wstg(J2)
      dw2RL =( ddist(J1)+wstg(J1))*xr(J1) +(-ddist(J2)+wstg(J2))*xr(J2)
      dw1RR =  ddist(J1)+wstg(J1)           +ddist(J2)+wstg(J2)
      dw2RR =( ddist(J1)+wstg(J1))*xr(J1) +( ddist(J2)+wstg(J2))*xr(J2)
C     
C We have two hits to get started, solve lin. regress.
      fdet = fw0*fw2-fw1*fw1
      IF(fdet.LE.0.) GO TO 999
      deti = 1./fdet
      dat0LL = ( dw1LL*fw2 - dw2LL*fw1)*deti
      slopLL = (-dw1LL*fw1 + dw2LL*fw0)*deti
      dat0LR = ( dw1LR*fw2 - dw2LR*fw1)*deti
      slopLR = (-dw1LR*fw1 + dw2LR*fw0)*deti
      dat0RL = ( dw1RL*fw2 - dw2RL*fw1)*deti
      slopRL = (-dw1RL*fw1 + dw2RL*fw0)*deti
      dat0RR = ( dw1RR*fw2 - dw2RR*fw1)*deti
      slopRR = (-dw1RR*fw1 + dw2RR*fw0)*deti
C     
C Loop over remaining wires, picking up hits for segment.
      DO 100 IL1 = 1,nlay
         IF(digi(1,IL1).LE.0.) GO TO 100
         IF(IL1.EQ.J1 .OR. IL1.EQ.J2) GO TO 100
C     
C Determine predicted distance, using dat0 and slop.
         predLL = dat0LL + slopLL*xr(IL1) - wstg(IL1)
         sgnLL  = SIGN(1.,predLL)
         predLR = dat0LR + slopLR*xr(IL1) - wstg(IL1)
         sgnLR  = SIGN(1.,predLR)
         predRL = dat0RL + slopRL*xr(IL1) - wstg(IL1)
         sgnRL  = SIGN(1.,predRL)
         predRR = dat0RR + slopRR*xr(IL1) - wstg(IL1)
         sgnRR  = SIGN(1.,predRR)
C     
         fw0  = fw0 + 1.
         fw1  = fw1 + xr(IL1)   ! Sum of the x es
         fw2  = fw2 + xr(IL1)*xr(IL1) ! Sum of the x squares
C     
         dsegLL = sgnLL*ddist(IL1) + wstg(IL1)
         dw1LL  = dw1LL + dsegLL
         dw2LL  = dw2LL + dsegLL*xr(IL1)
         dsegLR = sgnLR*ddist(IL1) + wstg(IL1)
         dw1LR  = dw1LR + dsegLR
         dw2LR  = dw2LR + dsegLR*xr(IL1)
         dsegRL = sgnRL*ddist(IL1) + wstg(IL1)
         dw1RL  = dw1RL + dsegRL
         dw2RL  = dw2RL + dsegRL*xr(IL1)
         dsegRR = sgnRR*ddist(IL1) + wstg(IL1)
         dw1RR  = dw1RR + dsegRR
         dw2RR  = dw2RR + dsegRR*xr(IL1)
C     
C If we have more than one hit in seg, solve lin. regress.
         fdet = fw0*fw2 - fw1*fw1
         IF(fdet.LE.0.) GO TO 100 ! avoid divding by delta=0
         deti = 1./fdet
         dat0LL = ( dw1LL*fw2 - dw2LL*fw1)*deti
         slopLL = (-dw1LL*fw1 + dw2LL*fw0)*deti
         dat0LR = ( dw1LR*fw2 - dw2LR*fw1)*deti
         slopLR = (-dw1LR*fw1 + dw2LR*fw0)*deti
         dat0RL = ( dw1RL*fw2 - dw2RL*fw1)*deti
         slopRL = (-dw1RL*fw1 + dw2RL*fw0)*deti
         dat0RR = ( dw1RR*fw2 - dw2RR*fw1)*deti
         slopRR = (-dw1RR*fw1 + dw2RR*fw0)*deti
 100  CONTINUE
C     
C Check if min. required layers is met
      IF (fw0.LT.xmin_nlay) GO TO 999
C     
C Get left/right ambig resol for the four solutions.
      chiLL = 0.
      chiLR = 0.
      chiRL = 0.
      chiRR = 0.
      LLL  = 0
      LLR  = 0
      LRL  = 0
      LRR  = 0
C
C "Lxx" has its first 6 bits set to one if the xx solution
C ends up on the RIGHT-hand side of the hit wire for each of the 6 layers.
      DO IL1=1,nlay
        IF(.NOT.miss(IL1)) THEN
          predLL = dat0LL + slopLL*xr(IL1) - wstg(IL1)
          IF(predLL.GT.0.) LLL = IBSET(LLL,IL1-1)
          drawLL = SIGN(ddist(IL1),predLL)
          chiLL = chiLL + ((drawLL - predLL)/derr(IL1))**2
C
          predLR = dat0LR + slopLR*xr(IL1) - wstg(IL1)
          IF(predLR.GT.0.) LLR = IBSET(LLR,IL1-1)
          drawLR = SIGN(ddist(IL1),predLR)
          chiLR = chiLR + ((drawLR - predLR)/derr(IL1))**2
C
          predRL = dat0RL + slopRL*xr(IL1) - wstg(IL1)
          IF(predRL.GT.0.) LRL = IBSET(LRL,IL1-1)
          drawRL = SIGN(ddist(IL1),predRL)
          chiRL = chiRL + ((drawRL - predRL)/derr(IL1))**2
C
          predRR = dat0RR + slopRR*xr(IL1) - wstg(IL1)
          IF(predRR.GT.0.) LRR = IBSET(LRR,IL1-1)
          drawRR = SIGN(ddist(IL1),predRR)
          chiRR = chiRR + ((drawRR - predRR)/derr(IL1))**2
        ENDIF
      ENDDO
C
C Record the slope differece with the best solution
C      IF (ABS(slopLL-slopTF).GT.Scut) chiLL = 100000.+chiLL
C      IF (ABS(slopLR-slopTF).GT.Scut) chiLR = 100000.+chiLR
C      IF (ABS(slopRL-slopTF).GT.Scut) chiRL = 100000.+chiRL
C      IF (ABS(slopRR-slopTF).GT.Scut) chiRR = 100000.+chiRR
C
C Cutting on slope difference where the window is a scaled chi**2 cut
C makes no sense.  Let's just add a component to chi**2 based on how much
C this differs from the Hit-Based slope with errors calculated in trk_dcam
C based on Hit-Based position errors.  This will be small unless the segment
C is far off from the fitted track.
C
      chiLL = chiLL + wgtslp*((slopLL-slopTF)**2)
      chiLR = chiLR + wgtslp*((slopLR-slopTF)**2)
      chiRL = chiRL + wgtslp*((slopRL-slopTF)**2)
      chiRR = chiRR + wgtslp*((slopRR-slopTF)**2)
C
C Normalize chi-square's by dividing by number of degrees of
C freedom for all four solutions, and correcting for the slope.
C      df = fw0 - 2.
      df = fw0 - 1.
      chiLL = chiLL/df
      chiLR = chiLR/df
      chiRL = chiRL/df
      chiRR = chiRR/df
C
C Pick the solution with the least chi-square
      chib = MIN(chiLL,chiLR,chiRL,chiRR)
C
C Save the set bits for the best solution and then pick the
C solution with the second best chi-square.
      IF(ABS(chib - chiLL).LT.0.00001) THEN
        LRB   = LLL
        slopB = slopLL
      ELSE IF(ABS(chib - chiLR).LT.0.00001) THEN
        LRB   = LLR
        slopB = slopLR
      ELSE IF(ABS(chib - chiRL).LT.0.00001) THEN
        LRB   = LRL
        slopB = slopRL
      ELSE IF(ABS(chib - chiRR).LT.0.00001) THEN
        LRB   = LRR
        slopB = slopRR
      ELSE
      ENDIF
      slopD = slopB - slopTF
C
C Mark the sign to indicate LR, "-" => Left of the wire.
      DO i=1,nlay
        IF (.NOT.miss(i)) THEN
          IF( BTEST(LRB,i-1) )THEN
            digi(4,i) =  ABS(digi(4,i))
          ELSE
            digi(4,i) = -ABS(digi(4,i))
          ENDIF
        ENDIF
      ENDDO
C
C If chi-square > 7.0 (CL < 1%), re-do LR_amb with 2 diff. starting layers
      IF(chib.LE.dpar_trk_lrambfit_chi2) GO TO 999
C
C The layers J1 & J2 failed, get started with other two layers,
      IF(istart.GT.0) THEN
        J1old(J1) = 1
        J1old(J2) = 1
C Reset "istart" only to re-do once with another 2 starting layers.
        istart = istart - 1
        DO il = 1, nlay
          miss(il) = .FALSE.
        ENDDO
        GO TO 30
      ENDIF
C
 401  format('Error getting DCGM bank # ',i1,'.')
  999 RETURN
      END

