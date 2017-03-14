      SUBROUTINE GEOM_CC
c     ================== 
c
c_begin_doc
c  RCS ID string
c  $Id: geom_cc.F,v 1.5 2008/03/16 04:33:18 fklein Exp $
c
c  Documentation for subroutine GEOM_CC
c
CA)========================================(begin of short description)
C    Defines geometry parameters and materials for
C    CC detector
CB)-----------------------------------------(begin of long description)
C
C    Created:   25-May-1995   Alexander Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C
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
C
#include "gconsp.inc"
#include "gcsets.inc"
#include "ccp.inc"
#include "ccph.inc"
#include "lcgeom.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_mg.inc"
#include "cccom.inc"
#include "cltmed.inc"
#include "ffpar_cc.inc"
C
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: geom_cc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:18 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/ 
     1'$Id: geom_cc.F,v 1.5 2008/03/16 04:33:18 fklein Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='geom_cc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
c
c  Local User defined variables:
c  -----------------------------
      real cc_parm(19)
c
c  Cerenkov support bar: added 5/13/05 (JWP)
c  From memory: 2" x 1" x width of Cerenkov, aluminum
      real ccbar_parm(3)
     
      integer med_air, mat_air, mat_number
      integer get_tmed, get_mate, get_rotm
      external  get_tmed, get_mate, get_rotm
c         
      data cc_parm/ -30.,  60.,  1., 5.,
     +    232., 276., 335.,
     +    339., 189., 335.,
     +    380., 130., 320.,
     +    420.,  40., 240.,
     +    510.,  40.,  45./

      data ccbar_parm/141.57, 2.5, 1.25/
c      
      data mat_air / 15 /
C
      INTEGER NOFSEC,NOFSP1
      PARAMETER( NOFSEC = 18 )  ! Number of CC segments
      PARAMETER( NOFSP1 = NOFSEC + 1 )
      REAL SR3
      PARAMETER( SR3 = 1.732050807569 ) ! SQRT(3.)
      REAL RMIN(NOFSEC), RMAX(NOFSEC), PHTRI(NOFSEC), PHTRA(NOFSEC)
      integer itest
      parameter (itest = 1)
c
c--------------------------------------------------------------------
C                                                                   !
C  *           ------------------------------ - RMAX    *           !
C   \        /                                \        /|           !
C    \      *                                  *      /PHTRA        !
C     \      \                                /      /              !
C      \      \                              /      /               !
C       \      \                            /PHATRA/                !
C        \      \                          /    | /                 !
C         \*-----*                        *-----*/ - RATRA          !
C          \\                                  //                   !
C           \\                                //                    !
C            \\                        PHATRI//                     !
C             \\                           |//                      !
C              **--------------------------** - RMIN                !
C                                           |                       !
C                                        PHTRI                      !
C                                                                   !
c-------------------------------------------------------------------!
c
C  Local variables
c
      INTEGER i,j,k,ICCROT(NOFSEC),iccroh(NOFSEC),ICCRMS(2*NOFSEC)
      integer ccbar_rot
c
      REAL THETAM,HTHETA,DR2,XSHIFT,ZSHIFT,PARCER(NOFSEC),TFI,TFA
      real paredh(5),parede(5),Rh0,xshh,zshh,alenh,phih,
     &          xsh, ysh, zsh, Re0,xshe,zshe,alene,phie
c
      real parsh1(3), parsh2(3)
      real x0ms(3),x1ms(3),x2ms(3),x3ms(3),x4ms(3),x5ms(3)
      real y0ms(3),y1ms(3),y2ms(3),y3ms(3),y4ms(3),y5ms(3)
      real uvect(3)
      real tet1,tet2,tet3,phi1,phi2,phi3
c
      integer ivol
c
C-----------------------------------------------------------------------
c
      DATA uvect /0.,0.,1./
C
C=================================================================
C
C--   DATA statements for  MATERIAL definitions
C
C
C--  Define Gas for CC ( C_4F_10 )
C
      INTEGER MXCCGS
      PARAMETER (MXCCGS = 2)
c
      REAL ACCGAS(MXCCGS), ZCCGAS(MXCCGS), WCCGAS(MXCCGS), DCCGAS
      DATA ACCGAS,         ZCCGAS,    WCCGAS,     DCCGAS
     &    /12.01,18.998,   6.0,9.0,  4.0,10.0,   0.009935/
C
C--  Define Freon 12 (C-Cl2-F2)
C
*      REAL AFREON(3), ZFREON(3), WFREON(3), DFREON
*      DATA AFREON  / 12.01,  35.45, 18.99 /
*      DATA ZFREON  /  6.0 ,  17.0 ,  9.0  /
*      DATA WFREON  /  1.0 ,   2.0 ,  2.0  /
*      DATA DFREON  /  0.00493 /
C
C--  Define mirror materials (CH)
C
      REAL ASCIN(2), ZSCIN(2), WSCIN(2), DNSCIN
      DATA   ASCIN/ 12.01, 1.01/
      DATA   ZSCIN/  6.00, 1.00/
      DATA   WSCIN/  1.00, 1.00/
      DATA   DNSCIN/ 1.035/
C
C--  Define "Magnetic shield materials"
C
c       For CC segments 1 - 10 and 13 - 18
c
        REAL  ASHMAT1(2), ZSHMAT1(2), WSHMAT1(2), DSHMAT1
C                    Fe  +  Cu
	DATA ASHMAT1 / 55.85,  63.546 /
	DATA ZSHMAT1 / 26.00,  29.00  /
	DATA WSHMAT1 / 0.7725, 0.2275 /
        DATA DSHMAT1 /  8.79 /
C
C     For CC segments 11 - 12
c
        REAL  ASHMAT2(3), ZSHMAT2(3), WSHMAT2(3), DSHMAT2
c
C                    Fe  +  CH
c
	DATA ASHMAT2 / 55.85, 12.01, 1.01 /
	DATA ZSHMAT2 / 26.00,  6.00, 1.00 /
	DATA WSHMAT2 / 0.8374, 0.1490, 0.0136 /
        DATA DSHMAT2 /  7.49 /
C
C ==========================================================
C
c_end_var
c
c  executable code for routine GEOM_CC
c------------------------------------------------------------------------------
c
c --- Gas for CC ---
c
      mat_cc_gas = get_mate()
      CALL GSMIXT(mat_cc_gas,'Gas for CC$',
     & ACCGAS,ZCCGAS,DCCGAS,-MXCCGS,WCCGAS)
c
c --- Mirror material ( close to CH ) ---
c
      mat_mirr_cc = get_mate()
      CALL GSMIXT(mat_mirr_cc,'CC mirror material $',
     &     ASCIN, ZSCIN,DNSCIN,-2, WSCIN)
c
C --- Magnetic shield material (1) ---
c
      mat_sh1 = get_mate()
      CALL GSMIXT(mat_sh1,'Magnet. Shield_1$',
     & ASHMAT1,ZSHMAT1,DSHMAT1,2,WSHMAT1)
C
C --- Magnetic shield material (2) ---
c
      mat_sh2 = get_mate()
      CALL GSMIXT(mat_sh2,'Magnet. Shield_2$',
     & ASHMAT2,ZSHMAT2,DSHMAT2,3,WSHMAT2)

c
C-----------------------------------------------------------------------
c      MEDIUM definitions
C-----------------------------------------------------------------------
c
***********************************************************************
* CALL GSTMED(ITMED,NATMED,NMAT,                                      *
*     ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)*
*  ITMED     tracking medium number 0<ITMED<100                       *
*  NATMED    tracking medium name (up to 20 characters ended by $)    *
*  NMAT      material number corresponding to ITMED                   *
*  ISVOL     =0 if not a sensitive volume                             *
*  IFIELD    = 0  if no magnetic field                                *
*            = -1  reserved for user decision in GUSWIM               *
*            = 1  tracking performed with GRKUTA                      *
*            = 2  tracking performed with GHELIX                      *
*            = 3  tracking performed with GHELX3                      *
*  FIELDM    maximum field value (in Kilogauss)                       *
*  TMAXFD    maximum  angle due  to field  permitted in  one step  (in*
*            degrees)                                                 *
*  STEMAX    Maximum step allowed                                     *
*            (in cm)                                                  *
*  DEEMAX    maximum fractional energy loss in one step               *
*  EPSIL     tracking precision (in cm)                               *
*  STMIN     minimum step  due to energy  loss or  multiple scattering*
*            (in cm)                                                  *
*  UBUF      array of NWBUF additional parameters                     *
*  NWBUF                                                              *
***********************************************************************
C Define the media.
C--  sensitive volume?
C--     |   magnetic field type
C--     |      |   maximum magnetic field [kG]
C--     |      |      |   maximal angle
C--     |      |      |      |   maximal dX for multiple scattering [cm]
C--     |      |      |      |      |   fract. eloss is meaningless since GEANT
C--     |      |      |      |      |      |   tracking precision
C--     |      |      |      |      |      |      |   minimum step
C--     \/     \/     \/     \/     \/     \/     \/     \/
c-------------------------------------------------------------------
c
      med_air = get_tmed()
      mat_number = mat_air
      if(nomate(cc))  mat_number = 16
      CALL GSTMED(med_air,'AIR $',   mat_number,
     &     0,INIT_IFIELD,INIT_FIELDM,
     1     1.,   0.5,    1.,   0.2,    2., 0, 0)
c ---
c
c --- Gas for CC ---
c
      med_cc_gas = get_tmed()
      mat_number = mat_cc_gas
      if(nomate(cc))  mat_number = 16
      CALL GSTMED(med_cc_gas,'GAS for CC$',   mat_number,
     &     1,INIT_IFIELD,INIT_FIELDM,
     1     1.,   20.0,    1.,   0.01,    2., 0, 0)
c ---
      med_mirr_cc = get_tmed()
      mat_number = mat_mirr_cc
      if(nomate(cc))  mat_number = 16
      CALL GSTMED( med_mirr_cc,'CC mirror $',  mat_number,
     &     0,INIT_IFIELD,INIT_FIELDM,
     1     1.,    0.5,    1.,   0.01,    0.2, 0, 0)
c ---
      med_sh1 = get_tmed()
      mat_number = mat_sh1
      if(nomate(cc))  mat_number = 16
      CALL GSTMED(med_sh1,'Magnetic shield - 1$', mat_number,
     &     0,INIT_IFIELD,INIT_FIELDM,
     1     2.,    0.5,    1.,   0.01,    0.1, 0, 0)
c ---
      med_sh2 = get_tmed()
      mat_number = mat_sh2
      if(nomate(cc))  mat_number = 16
      CALL GSTMED(med_sh2,'Magnetic shield - 2$', mat_number,
     &     0,INIT_IFIELD,INIT_FIELDM,
     1     2.,    0.5,    1.,   0.01,    0.1, 0, 0)
c ---
c
c    Possible (BUT NOT DESIRED !!! A.V.) change of medium cuts
c    =========================================================
c
      call set_tpar( med_air,    cc_cuts)
      call set_tpar( med_cc_gas, cc_cuts)
      call set_tpar( med_mirr_cc,cc_cuts)
      call set_tpar( med_sh1,    cc_cuts)
      call set_tpar( med_sh2,    cc_cuts)
c
C====================================================================
c
      CALL CCDEIN
c     ===========
c
C====================================================================
c
c  Define mother volume for cerenkov (PGON)
c
c ---
      if(helium_only) then
        call gsvolu('CC  ','PGON',clas_med_helium,cc_parm,19,ivol)
      else
        call gsvolu('CC  ','PGON',med_air,cc_parm,19,ivol)
      end if
      call gspos('CC  ',1,'LSS-',0.,0.,0.,0,'MANY')	
C
C ==========================================================
C
C     Put CC-counts ('CCxx') into mother volume 'CC  '
C
      CALL GSVOLU ('CCVO', 'TRD2' , med_cc_gas , PARCER , 0 , IVOL )
      CALL GSVOLU ('CCHP', 'TUBS' , med_mirr_cc, PAREDH , 0 , IVOL )
      CALL GSVOLU ('CCEP', 'TUBS' , med_mirr_cc, PAREDE , 0 , IVOL )
      CALL GSVOLU ('CCHL', 'TUBS' , med_mirr_cc, PAREDH , 0 , IVOL )
      CALL GSVOLU ('CCEL', 'TUBS' , med_mirr_cc, PAREDE , 0 , IVOL )
c
      call GSVOLU ('CCS1', 'BOX ' , med_sh1 , PARSH1 , 0 , IVOL )
      call GSVOLU ('CCS2', 'BOX ' , med_sh2 , PARSH2 , 0 , IVOL )
c
      DO i=1,NOFSEC
C     =============
C
        RMIN(i) = Y21(i) - 0.5
        RMAX(i) = Y12(i) + 0.5
C
        THETAM  = th(i)
        TFI=RMIN(i)*SIN(THETAM*DEGRAD)/SR3
        TFA=RMAX(i)*SIN(THETAM*DEGRAD)/SR3
        HTHETA=TAN((THETAM-THMIN(i))*DEGRAD)
        DR2 = (RMAX(i)-RMIN(i)) / 2.
        XSHIFT=(DR2 + RMIN(i))*SIN(THETAM*DEGRAD)
        ZSHIFT=(DR2 + RMIN(i))*COS(THETAM*DEGRAD)
C
        PHTRI(i) = TFI - 4.5
        PHTRA(i) = TFA - 4.5
C
C --- Definition of cherenkov counter
C
        PARCER (1)=PHTRI(i)
        PARCER (2)=PHTRA(i)
        PARCER (3)=RMIN(i)*HTHETA
        PARCER (4)=RMAX(i)*HTHETA
        PARCER (5)=DR2
 
        Re0 = 280.
        alene = PHTRA(i)/2.
        phie  = raddeg*atan(alene/Re0)
        parede(1) = Re0 - 0.35
        parede(2) = Re0
        parede(3) = RMAX(i)*HTHETA
        parede(4) = -phie
        parede(5) =  phie
        zshe = DR2 - Re0
        xshe = alene
 
        Rh0 = 210.
        alenh = PHTRI(i)/2.
        phih  = raddeg*asin(alenh/Rh0)
        paredh(1) = Rh0
        paredh(2) = Rh0 + 0.3
        paredh(3) = RMIN(i)*HTHETA
        paredh(4) = -phih
        paredh(5) =  phih
        zshh      = -DR2 - sqrt(Rh0*Rh0 - alenh*alenh)
        xshh = alenh
C
C --- Define rotation to position CC section
c=====================================================
c
        CALL SGNROT(ICCROT(i))
        CALL GSROTM(ICCROT(i),90.,90.,THETAM-90.,0.,THETAM,0.)
        CALL GSPOSP( 'CCVO', i,'CC  ',XSHIFT,0.,ZSHIFT,ICCROT(i),
     &    'MANY', PARCER, 5)
c=====================================================
        XSH=(RMAX(i)-Re0)*SIN(THETAM*DEGRAD)
        YSH=alene
        ZSH=(RMAX(i)-Re0)*COS(THETAM*DEGRAD)
C
        CALL SGNROT(ICCROH(i))
C       ======================
        CALL GSROTM(ICCROH(i),THETAM,0.,90.,90.,THETAM-90.,0.)
        CALL GSPOSP( 'CCEP', i,'CC  ',xsh,ysh,zsh,
     &    ICCROH(i),'ONLY', parede, 5)
C
        YSH=-alene
        CALL GSPOSP( 'CCEL', i,'CC  ',xsh,ysh,zsh,
     &    ICCROH(i),'ONLY', parede, 5)
c=====================================================
        XSH=(DR2 + RMIN(i)+zshh)*SIN(THETAM*DEGRAD)
        YSH=alenh
        ZSH=(DR2 + RMIN(i)+zshh)*COS(THETAM*DEGRAD)
C
        CALL GSPOSP( 'CCHP', i,'CC  ',xsh,ysh,zsh,
     &    ICCROH(i),'ONLY', paredh, 5)
C
        YSH=-alenh
        CALL GSPOSP( 'CCHL', i,'CC  ',xsh,ysh,zsh,
     &    ICCROH(i),'ONLY', paredh, 5)
c
c=====================================================
c  Define the Magnetic shield walls
c
c  Center of MS box
c
        call ucopy(pcms0(1,1,i),x0ms,3)
c
c  Centers of walls
c
        call ucopy(pcms0(1,2,i),x1ms,3)
        call ucopy(pcms0(1,3,i),x2ms,3)
        call ucopy(pcms0(1,4,i),x3ms,3)
        call ucopy(pcms0(1,5,i),x4ms,3)
        call ucopy(pcms0(1,6,i),x5ms,3)
c
c  Remember now, that there are two sides of CC segment !
c
        call ucopy(x0ms,y0ms,3)
        call ucopy(x1ms,y1ms,3)
        call ucopy(x2ms,y2ms,3)
        call ucopy(x3ms,y3ms,3)
        call ucopy(x4ms,y4ms,3)
        call ucopy(x5ms,y5ms,3)
c
        y0ms(1) = -x0ms(1)
        y1ms(1) = -x1ms(1)
        y2ms(1) = -x2ms(1)
        y3ms(1) = -x3ms(1)
        y4ms(1) = -x4ms(1)
        y5ms(1) = -x5ms(1)
c
c  Now transfer that coordinates to the SG R.S.
c
c   From CC to SG Reference System :
c
        call CCTOSG(i,x0ms,x0ms)
        call CCTOSG(i,x1ms,x1ms)
        call CCTOSG(i,x2ms,x2ms)
        call CCTOSG(i,x3ms,x3ms)
        call CCTOSG(i,x4ms,x4ms)
        call CCTOSG(i,x5ms,x5ms)
        call CCTOSG(i,y0ms,y0ms)
        call CCTOSG(i,y1ms,y1ms)
        call CCTOSG(i,y2ms,y2ms)
        call CCTOSG(i,y3ms,y3ms)
        call CCTOSG(i,y4ms,y4ms)
        call CCTOSG(i,y5ms,y5ms)
c
c   Now we are to define magnetic shield walls in GEANT:
c
        call CCROTP(x0ms,x1ms,x2ms,tet1,tet2,tet3,phi1,phi2,phi3)
c
        k = 2*i - 1
        CALL SGNROT(ICCRMS(k))
        CALL GSROTM(ICCRMS(k),tet1,phi1,tet2,phi2,tet3,phi3)
c
c    The other side of CC segment:
c
        call CCROTP(y0ms,y1ms,y2ms,tet1,tet2,tet3,phi1,phi2,phi3)
c
        k = 2*i
*        ICCRMS(k) = get_rotm() + 20
        CALL SGNROT(ICCRMS(k))
        CALL GSROTM(ICCRMS(k),tet1,phi1,tet2,phi2,tet3,phi3)
c
C ================================================================
c
        if(i.ge.1.and.i.le.10) then
c
c ------   Variant 1 ------ * 9.93 * 7.76 * 21.08 *** l eff = 0.455
c
          parsh1(3) = 21.08
          k = 2*i - 1
c
          j = 8*(i-1) + 1
          parsh1(1) =  0.455
          parsh1(2) =  7.76
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x1ms(1),x1ms(2),x1ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 2
          parsh1(1) =  9.93
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x2ms(1),x2ms(2),x2ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 3
          parsh1(1) =  0.455
          parsh1(2) =  7.76
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x3ms(1),x3ms(2),x3ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 4
          parsh1(1) =  9.93
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x4ms(1),x4ms(2),x4ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          k = 2*i
c
          j = 8*(i-1) + 5
          parsh1(1) =  0.455
          parsh1(2) =  7.76
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y1ms(1),y1ms(2),y1ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 6
          parsh1(1) =  9.93
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y2ms(1),y2ms(2),y2ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 7
          parsh1(1) =  0.455
          parsh1(2) =  7.76
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y3ms(1),y3ms(2),y3ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-1) + 8
          parsh1(1) =  9.93
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y4ms(1),y4ms(2),y4ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
C ================================================================
c
        else if(i.ge.11.and.i.le.12) then
c
c ------   Variant 2 ------ * 11.46 * 8.46 * 24.13 *** l eff = 0.493
c
c
          parsh2(3) = 24.13
          k = 2*i - 1
c
          j = 8*(i-11) + 1
          parsh2(1) =  0.493
          parsh2(2) =  8.46
c
          CALL GSPOSP( 'CCS2', j,'CC  ',x1ms(1),x1ms(2),x1ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 2
          parsh2(1) = 11.46
          parsh2(2) =  0.493
c
          CALL GSPOSP( 'CCS2', j,'CC  ',x2ms(1),x2ms(2),x2ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 3
          parsh2(1) =  0.493
          parsh2(2) =  8.46
c
          CALL GSPOSP( 'CCS2', j,'CC  ',x3ms(1),x3ms(2),x3ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 4
          parsh2(1) = 11.46
          parsh2(2) =  0.493
c
          CALL GSPOSP( 'CCS2', j,'CC  ',x4ms(1),x4ms(2),x4ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          k = 2*i
c
          j = 8*(i-11) + 5
          parsh2(1) =  0.493
          parsh2(2) =  8.46
c
          CALL GSPOSP( 'CCS2', j,'CC  ',y1ms(1),y1ms(2),y1ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 6
          parsh2(1) = 11.46
          parsh2(2) =  0.493
c
          CALL GSPOSP( 'CCS2', j,'CC  ',y2ms(1),y2ms(2),y2ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 7
          parsh2(1) =  0.493
          parsh2(2) =  8.46
c
          CALL GSPOSP( 'CCS2', j,'CC  ',y3ms(1),y3ms(2),y3ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
          j = 8*(i-11) + 8
          parsh2(1) = 11.46
          parsh2(2) =  0.493
c
          CALL GSPOSP( 'CCS2', j,'CC  ',y4ms(1),y4ms(2),y4ms(3),
     &    ICCRMS(k),'ONLY', parsh2, 3)
c
C ================================================================
c
        else if(i.ge.13.and.i.le.18) then
c
c ------   Variant 3 ------ * 12.65 * 9.85 * 27.94 *** l eff = 0.455
c
          parsh1(3) = 27.94
          k = 2*i - 1
c
          j = 8*(i-13+10) + 1
          parsh1(1) =  0.455
          parsh1(2) =  9.85
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x1ms(1),x1ms(2),x1ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 2
          parsh1(1) = 12.65
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x2ms(1),x2ms(2),x2ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 3
          parsh1(1) =  0.455
          parsh1(2) =  9.85
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x3ms(1),x3ms(2),x3ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 4
          parsh1(1) = 12.65
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',x4ms(1),x4ms(2),x4ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          k = 2*i
c
          j = 8*(i-13+10) + 5
          parsh1(1) =  0.455
          parsh1(2) =  9.85
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y1ms(1),y1ms(2),y1ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 6
          parsh1(1) = 12.65
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y2ms(1),y2ms(2),y2ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 7
          parsh1(1) =  0.455
          parsh1(2) =  9.85
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y3ms(1),y3ms(2),y3ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
          j = 8*(i-13+10) + 8
          parsh1(1) = 12.65
          parsh1(2) =  0.455
c
          CALL GSPOSP( 'CCS1', j,'CC  ',y4ms(1),y4ms(2),y4ms(3),
     &    ICCRMS(k),'ONLY', parsh1, 3)
c
C ================================================================
c
        end if
c
c==========================================================
      ENDDO

c  Place Cerenkov support bar.
      call gsvolu ('CCSB',
     $     'BOX ',
     $     clas_med_alu,
     $     ccbar_parm,
     $     3,
     $     IVOL)
      call sgnrot (ccbar_rot)
      call gsrotm (ccbar_rot, 90., 90., 45., 0., 45., 0.)
      call gspos ('CCSB', 1, 'CC  ', 262.50, 0., 264.65, ccbar_rot, 'ONLY')

c
      CALL GSATT( 'CCVO', 'COLO',6)  ! color attribute
      CALL GSATT( 'CCHP', 'COLO',4)  ! color attribute
      CALL GSATT( 'CCHL', 'COLO',4)  ! color attribute
      CALL GSATT( 'CCEP', 'COLO',4)  ! color attribute
      CALL GSATT( 'CCEL', 'COLO',4)  ! color attribute
      CALL GSATT( 'CCS1', 'COLO',3)  ! color attribute
      CALL GSATT( 'CCS2', 'COLO',3)  ! color attribute
C
C>>>>>>>>>>>>>>>>>>>>>>>>>>
C
      return
      end
