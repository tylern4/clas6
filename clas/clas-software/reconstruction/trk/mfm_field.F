      SUBROUTINE MFM_FIELD(X,F)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_field.F,v 1.3 1999/04/05 01:05:42 kossov Exp $
c
c  Documentation for subroutine MFM_FIELD
c
c  Purpose: Calculate magnetic field. According Bio-Savarta law
C  -------  it is summed over all straight current elements in
C           a loop. The coil may consist of many loops.
c
c  Calling Sequence: in case of Big_ or Mini_Torus calls MFM_TORFIELD
c  ----------------- in case of Polarized Target calls MFM_PTGFIELD
c
c  Input Parameters: X(3), MFM_MAG_type=1 - BigTor, =2 - MiniTor, =3 - PolTarg
c  ----------------
c
c  Output Parameters: F(3)  - Bx,By,Bz magnetic field
c  -----------------
c
c  Called from: MFM_BRUN
c  ------------
c
c  Other routines: NONE
c  ---------------
c
c  Notes: Remember that correct magnetic field shapes for the Polarized Target
c  ------ & the current ration is still not provided by Oxford! Magnetic field
c         of Big_ and Mini_Torus is stil "theoretical" not measured
c
c  Author:   Mikhail Kossov      Created:  Mon May 12 14:01:12 EST 1997
c  -------   developed from "torus" program of Bogdan (SDA)
c
c  Last Revision: M.Kossov 3/24/99 - double precision & symmetry are improved
c                                    coils are smothed, divn is checked
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
#include "mfm_control.inc"
C -- added to communicate with TRK parameters -- M.K. - 17-Jan-98
#include "trktcl_var.inc" 
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL X(3), F(3)
C
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_field.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/04/05 01:05:42 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_field')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER i,j,k,m,io,No,Nox,il,nloops,is,nshape,nlY,nlX, ilY,ilX
      INTEGER nlT,ilT, nlR,ilR               ! M.K.
      REAL ymin,zmin,ymax,zmax,phmin,phmax,phi,dphi,R0,Ri,dR,ddR 
      REAL*8 BT(3),dT,ddT,dT0,RiS,T30,S60                             ! M.K.
      REAL dX0, dX, ddX, R, Z, HPTG0, BR, BZ, y(3), sp, cp
      PARAMETER (HPTG0 = 50.)
      REAL XNORM1, XNORM2, XNORM3              
      LOGICAL   LFIRST
      DATA      LFIRST /.TRUE./
      INTEGER   nsect
      PARAMETER (nsect=6)
      REAL*8 piby2, pi, pi3, twopi, sinsec(nsect), cossec(nsect)
C
C Big Torus
      PARAMETER (No = 12   )
      PARAMETER (dX = 4.53 )   
      PARAMETER (dR = 22.923)
      PARAMETER (nlY = 54, nlX = 4)
C      PARAMETER (nloops = nlY*nlX, nshape = 105)
      PARAMETER (nloops = nlY*nlX, nshape = 137)
C
      INTEGER ISX(6),ISY(6)
      DATA ISX /0,-2,-2, 0, 2, 2/
      DATA ISY /3, 1,-1,-3,-1, 1/
C
      INTEGER ndiv(No),nord(No)
      REAL shape(3,nshape,nloops), oxford(6,No) 
      REAL SU(3,nshape)
C
C      DATA ndiv /21,  9,  1,  8,  9, 11, 20,  1, 14,  1, 10,  0/
      DATA ndiv  /30, 18,  1, 16, 12, 12, 20,  1, 14,  1, 12,  0/
      DATA nord /3,2,1,11,10,4,5,6,7,8,9,0/    ! order of corrections
C
C Mini Torus
      INTEGER Nor, nloopr, nshaper
      REAL dRr, ddRr, y0,z0
      PARAMETER (Nor = 8   )   
      PARAMETER (dRr = 9.82)
      PARAMETER (nlR = 5, nlT = 4)
C      PARAMETER (nloopr = 5, nshaper = 35)
      PARAMETER (nloopr = nlr*nlT, nshaper = 64)
      INTEGER ndivr(Nor)
      REAL shaper(3,nshaper,nloopr), rmint(6,Nor)
      REAL SUr(3,nshaper)
C
C      DATA ndivr /5, 5, 1, 11, 10, 1, 1, 1/
      DATA ndivr /9, 9, 1, 21, 21, 1, 1, 1/
C
C Shape of outer loop of BigTorus in coil plane [cm] (Oxford Data).
C     Flag     Y0      Z0       R0      Ystart   Zstart
C        1      2       3        4        5        6
C     -------------------------------------------------
      DATA oxford /
     1   1.,  -4.00,   0.30,  129.20,   102.5382,  -72.7906,
     2   1.,  66.80, 163.94,   49.10,    47.3032,  118.8775,        
     3   0.,  66.80, 163.94,   49.10,    17.7000,  163.9406,     
     4   1.,  66.80, 258.21,   49.10,    17.7000,  258.2077,
     5   1.,  77.06, 240.46,   69.60,    42.2260,  300.7158,
     6   1., -27.52,  35.16,  300.00,   108.6514,  302.4773,
     7   1.,  -2.50,  48.81,  271.50,   235.8441,  178.8268,
     8   0.,  -2.50,  48.81,  271.50,   214.2994, -114.6228,
     9   1., 153.12, -86.47,   65.80,   205.6606, -126.0824,
     &   0., 153.12, -86.47,   65.80,   113.3277, -138.8793,      
     1   1., 142.04, -99.89,   47.90,   108.3037, -133.8959,        
C     ---0---------0---------0---------0---------0---------0---------0-2
     2   1.,  -4.00,   0.30,  129.20,   102.5382,  -72.7906/
C
C Shape of outer loop of MiniTorus in coil plane [cm] (Russion Data).
C     Flag     Y0      Z0       R0      Ystart   Zstart
C        1      2       3        4        5        6
C     -------------------------------------------------
      DATA rmint /
     1   1.,   1.95,  0.081,   15.05,    16.99,    0.71,
     2   1.,  16.92,  24.56,   13.62,     9.82,   12.93,             
     3   0.,  16.92,  24.56,   13.62,     3.30,   24.55,     
     4   1.,  16.92,  52.30,   13.62,     3.30,   52.30,
     5   1., -29.53,   0.00,   83.56,    25.96,   62.48,
     6   0., -29.53,   0.00,   83.56,    54.05,    0.00,
     7   0.,   0.00,   0.00,    0.00,    54.05, -200.00,
     8   0.,   0.00,   0.00,    0.00,    16.99, -200.00/
c
c_end_var
c
c  executable code for routine MFM_FIRELD:
c  -----------------------------------------------------------------------
C
      IF (LFIRST) THEN
        LFIRST = .FALSE.
C        WRITE(CRMESS,*)'First call MFM_MAG_type=',MFM_MAG_type
        WRITE(CRMESS,*)'First call ipar_trk_magtyp=',ipar_trk_magtyp
        CALL RECMES(CRNAME,'I',CRMESS)
C
        pi    = DABS(DACOS(-1.D0))
        S60   = DSQRT(3.D0)/2.D0
        piby2 = pi/2.D0
        pi3   = pi/3.D0
        twopi = pi + pi
C Correction of the touch-points for the circles
c        j=0
c        DO i=1,No ! centers 3 & 4 are firm
c          j=j+ndiv(i)
c          io=nord(i)
c          IF(io.GT.0.AND.io.LT.10.AND.io.NE.8) THEN
c            il=nord(i-1)
c            is=nord(i+1)
c            print*,'io=',io,':             il=',il,', is=',is
c            IF(is.GT.0) THEN  ! Not the last calculation
c              sp=oxford(2,io)-oxford(2,il)  ! Y shift between points
c              cp=oxford(3,io)-oxford(3,il)  ! Z(X) sh between points
c              IF(io.EQ.3.OR.io.EQ.4) THEN   ! Just check
c                IF(oxford(6,io).EQ.oxford(3,io)) THEN
c                  R0=oxford(2,io)-oxford(5,io)
c                  IF(ABS(R0-oxford(4,io)).GT.0.00001) THEN
c                    IF(io.EQ.3) THEN        ! Should not happen
c                      print*,'MFM_FIELD: corrected R23=',R0,oxford(4,io)
c                    ELSE
c                      print*,'MFM_FIELD: corrected R45=',R0,oxford(4,io)
c                    ENDIF
c                  ENDIF
c                  print*,'i=',io,':Rio_new=',R0,',Rio_old=',oxford(4,io)
c                  oxford(4,io)=R0
c                ELSE
c                  R0=SQRT((oxford(6,io)-oxford(3,io))**2+
c     *                    (oxford(5,io)-oxford(2,io))**2)  
c                  IF(io.EQ.3) THEN        ! Should not happen
c                    print*,'MFM_FIELD:3',oxford(3,io),'#z=',oxford(6,io)
c                  ELSE
c                    print*,'MFM_FIELD:4',oxford(3,io),'#z=',oxford(6,io)
c                  ENDIF
c                  print*,'i=',io,':Rio_new=',R0,',Rio_old=',oxford(4,io)
c                  oxford(4,io)=R0
c                ENDIF
c              ELSE IF(io.GT.4.AND.io.LT.8) THEN ! No concurved circles
c                ddR=SQRT(sp*sp+cp*cp)-ABS(oxford(4,il)-oxford(4,io))
cC                print*,sp,cp,'4l,o=',il,io,oxford(4,il),oxford(4,io),ddR
c                IF(oxford(4,il).GT.oxford(4,io)) THEN
c                  R0=oxford(4,io)+ddR
c                ELSE
c                  R0=oxford(4,io)-ddR
c                ENDIF
c                print*,'io=',io,': Rio_new=',R0,',Rio_old=',oxford(4,io)
c                oxford(4,io)=R0
c              ELSE IF(io.LT.3) THEN ! Concurved circle exists
c                sp=oxford(2,io)-oxford(2,is) ! Y shift between points
c                cp=oxford(3,io)-oxford(3,is) ! Z(X) sh between points
c                ddR=SQRT(sp*sp+cp*cp)-ABS(oxford(4,is)+oxford(4,io))
cC                print*,sp,cp,'is,o=',is,io,oxford(4,is),oxford(4,io),ddR
c                R0=oxford(4,is)+ddR
c                print*,'io=',io,': Ris_new=',R0,',Ris_old=',oxford(4,is)
c                oxford(4,is)=R0
c              ENDIF
c              IF(io.LT.3.OR.(io.GT.4.AND.io.LT.8)) THEN ! Circle crossing correction
c                phi=ATAN2(sp,cp)
c                IF(io.EQ.1) phi=phi+pi
cC                print*,'io=',io,', phi=',phi,'sin,cos=',SIN(phi),COS(phi)
c                IF(oxford(5,io)-oxford(2,io).GT.0.) THEN
c                  sp=oxford(2,io)+oxford(4,io)*ABS(SIN(phi))
c                ELSE
c                  sp=oxford(2,io)-oxford(4,io)*ABS(SIN(phi))
c                ENDIF
c                IF(oxford(6,io)-oxford(3,io).GT.0.) THEN
c                  cp=oxford(3,io)+oxford(4,io)*ABS(COS(phi))
c                ELSE
c                  cp=oxford(3,io)-oxford(4,io)*ABS(COS(phi))
c                ENDIF
c                print*,io,',yo=',sp,oxford(5,io),',zo=',cp,oxford(6,io)
c                oxford(5,io)=sp
c                oxford(6,io)=cp
c              ENDIF
cc        0---------0---------0---------0---------0---------0---------0-2
c            ELSE IF(io.EQ.9) THEN
c              oxford(4,12)=oxford(4,1)
c              oxford(5,12)=oxford(5,1)
c              oxford(6,12)=oxford(6,1)
c              oxford(4, 8)=oxford(4,7)
c              sp=oxford(5,8)-oxford(2,8)  ! Y shift between points
c              cp=oxford(6,8)-oxford(3,8)  ! Z(X) sh between points
c              phi=ATAN2(sp,cp)
c              sp=oxford(2,8)+oxford(4,8)*SIN(phi)
c              cp=oxford(3,8)+oxford(4,8)*COS(phi)
c              print*,'8 y=',sp,oxford(5,8),', z=',cp,oxford(6,8)
c              oxford(5,8)=sp
c              oxford(6,8)=cp
c              sp=oxford(5,11)-oxford(2,11)  ! Y shift between points
c              cp=oxford(6,11)-oxford(3,11)  ! Z(X) sh between points
c              phi=ATAN2(sp,cp)
c              sp=oxford(2,11)+oxford(4,11)*SIN(phi)
c              cp=oxford(3,11)+oxford(4,11)*COS(phi)
c              print*,'11 y=',sp,oxford(5,11),', z=',cp,oxford(6,11)
c              oxford(5,11)=sp
c              oxford(6,11)=cp
c              il=io-2 ! The previous circle
c              is=io+2 ! The next circle
c              sp=oxford(2,io)-oxford(2,il)  ! Y shift between points
c              cp=oxford(3,io)-oxford(3,il)  ! Z(X) sh between points
c              phmin=ATAN2(sp,cp)
c              print*,'dR1=',oxford(4,io)-oxford(4,il),SQRT(sp*sp+cp*cp)
c              phi=ASIN((oxford(4,io)-oxford(4,il))/SQRT(sp*sp+cp*cp))
c              print*,'gamma1=',phi,', betta1=',phmin,', d=',phi-phmin
c              sp=oxford(2,io)-oxford(2,is)  ! Y shift between points
c              cp=oxford(3,io)-oxford(3,is)  ! Z(X) sh between points
c              phmax=ATAN2(sp,cp)
c              print*,'dR2=',oxford(4,io)-oxford(4,is),SQRT(sp*sp+cp*cp)
c              dphi=ASIN((oxford(4,io)-oxford(4,is))/SQRT(sp*sp+cp*cp))
c              print*,'gamma2=',dphi,', betta2=',phmax,', d=',dphi-phmax
c            ENDIF
c          ENDIF ! End of io=0 IF
c        ENDDO
c        IF(j.NE.nshape) THEN
c          PRINT*,'MFM_FIELD:***FATAL***SUM=',j,' # nshape=',nshape
c          STOP
c        ENDIF
c        DO i=1,No
c          PRINT 2700,i,(oxford(j,i),j=2,6)
c          oxford(2,i)=oxford(2,i)+.5      !!!!!!!!!!!!! y-SHIFT !!!!!!!!!!!!!
c          oxford(5,i)=oxford(5,i)+.5      !!!!!!!!!!!!! y-SHIFT !!!!!!!!!!!!!
c 2700     FORMAT('i=',i2,5F12.5)
c        ENDDO
C
C Normalize Polarized target magnetic field
C        IF (MFM_MAG_type .EQ. 3) THEN
        IF (ipar_trk_magtyp .EQ. 3) THEN
          R=0.
          Z=0.
          CALL MFM_PTGFIELD(R,Z,BR,BZ)
          IF (BZ .EQ. 0. ) THEN
            WRITE(CRMESS,*)'*** BZ0=',BZ,'(!), BR0=',BR
            CALL RECMES(CRNAME,'F',CRMESS)
          ENDIF
          XNORM3=HPTG0/ABS(BZ)
          WRITE(CRMESS,*)'XNORM3=',XNORM3,', MaxBZ=',ABS(BZ),', BR(0,0)=',BR
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF
C
C Initialize array for rotation to sectors
        DO i = 1,6
C          sinsec(i) = DSIN(FLOAT(i-1) * pi3)
C          cossec(i) = DCOS(FLOAT(i-1) * pi3)
          IF (ISX(i).EQ.0) THEN
            sinsec(i) = 0.D0
          ELSE IF (ISX(i).EQ.-2) THEN
            sinsec(i) =-S60
          ELSE IF (ISX(i).EQ. 2) THEN
            sinsec(i) = S60
          ENDIF
C
          IF (ISY(i).EQ.3) THEN
            cossec(i) =  1.D0
          ELSE IF (ISY(i).EQ.-3) THEN
            cossec(i) = -1.D0
          ELSE IF (ISY(i).EQ. 1) THEN
            cossec(i) = 0.5D0
          ELSE IF (ISY(i).EQ.-1) THEN
            cossec(i) =-0.5D0
          ENDIF
        ENDDO
C
C Define the normalization constants for the big Torus and mini Torus:
C   Big Torus 
C       Imax_t = 3860 [A] 
C       Nloops = 4*54 = 216 [No of loops in a sector]
C       Xnorm1 = 216*3860*E-4 = 83.376 [A Turns]
C
        XNORM1 = 83.376/nloops
C
C   Mini Torus
C       Imax_m = 8000 [A]
C       Nloops = 5 [No of loops in a sector]
C       Xnorm2 = 5*8000E-4 = 4.0 [A Turns]
C        
        XNORM2 = 4.0/nloopr
C
C
C Loop over the Oxford nodes ( 8 circles + 3 straights)
C The 12th node was added to close the loop
C
C        IF(MFM_MAG_type.EQ.1) THEN
        IF(ipar_trk_magtyp.EQ.1) THEN
          ddR = dR/nlY
          ddX = dX/nlX
          dX0 = -dX/2. - ddX/2.         
          is = 0
          Nox = No -1
          DO io = 1,Nox
            IF(oxford(1,io).EQ.1.) THEN
C Circle sections
              ymin = oxford(5,io) - oxford(2,io)
              zmin = oxford(6,io) - oxford(3,io)
              phmin = ATAN2(ymin,zmin)
              ymax = oxford(5,io+1) - oxford(2,io)
              zmax = oxford(6,io+1) - oxford(3,io)
              phmax = ATAN2(ymax,zmax)
              IF(io.EQ.9) phmax = phmax + twopi
              dphi = ABS(phmin-phmax)/ndiv(io)
            ELSE
C Straight sections 
              ymin = oxford(5,io) - oxford(2,io)
              zmin = oxford(6,io) - oxford(3,io)
              phmin = ATAN2(ymin,zmin)
              phmax = 0.
              dphi  = 0.
            ENDIF
c
            DO i = 1,ndiv(io)
              is = is + 1
              IF(io.EQ.1) THEN
                phi= phmin + (1-i)*dphi 
                R0 = oxford(4,io) - ddR/2.
              ELSE
                phi= phmin + (i-1)*dphi 
                R0 = oxford(4,io) + ddR/2.
              ENDIF
              il = 0
              DO ilX = 1,nlX
                DO ilY = 1,nlY
                  il = il + 1      
                  Ri = R0 - ilY*ddR
                  IF(io.EQ.1) Ri = R0 + ilY*ddR 
                  shape(1,is,il) = dX0 + ilX*ddX
                  shape(2,is,il) = oxford(2,io) + Ri*SIN(phi) 
                  shape(3,is,il) = oxford(3,io) + Ri*COS(phi)
                ENDDO             ! EndDo over nlY 
              ENDDO               ! EndDo over nlX
            ENDDO                 ! EndDo over ndiv
          ENDDO                   ! EndDo over Nox
        ENDIF                     ! End IF(MFM_MAG_type.EQ.1)
C
C Loop over the Oxford nodes (4 circles + 4 straights)
C
C        IF(MFM_MAG_type.EQ.2) THEN
        IF(ipar_trk_magtyp.EQ.2) THEN
          ddRr = dRr/nlR
C
C Calculate the PHI dimension of the Russion Torus
          T30  = DTAN( pi/6.D0) ! T(30deg)
          dT   = T30/8.D0       ! Doubled TAN(PHI) step of the optimal grid
          dT0  = dT/nlT         ! Half step betwin conducters in TAN(PHI)
          ddT  = dT0 + dT0      ! A step betwin conductors in TAN(PHI)
          dT0  = - dT - dT0     ! Starting point for TAN(PHI) sequence
          is = 0
          Nox = Nor
          DO io = 1,Nox
            IF(io.LE.5) THEN
              IF(rmint(1,io).EQ.1.) THEN
C Circle sections
                ymin = rmint(5,io) - rmint(2,io)
                zmin = rmint(6,io) - rmint(3,io)
                phmin = ATAN2(ymin,zmin)
                ymax = rmint(5,io+1) - rmint(2,io)
                zmax = rmint(6,io+1) - rmint(3,io)
                phmax = ATAN2(ymax,zmax)
                dphi = ABS(phmin-phmax)/ndivr(io)
              ELSE
C Straight sections 
                ymin = rmint(5,io) - rmint(2,io)
                zmin = rmint(6,io) - rmint(3,io)
                phmin = ATAN2(ymin,zmin)
                phmax = 0.
                dphi  = 0.
              ENDIF
c
              DO i = 1,ndivr(io)
                is = is + 1
                IF (io .EQ. 1) THEN
                  phi= phmin + (1-i)*dphi 
                  R0 = rmint(4,io) - ddRr/2.
                ELSE
                  phi= phmin + (i-1)*dphi 
                  R0 = rmint(4,io) + ddRr/2.
                ENDIF
                il = 0
                DO ilT = 1,nlT
                  DO ilR = 1,nlR      
C                  DO ilR = 1,nloopr
                    il = il + 1
                    IF (io .EQ. 1) THEN
                      Ri = R0 + ilR*ddRr
                    ELSE
                      Ri = R0 - ilR*ddRr
                    ENDIF
                    RiS = rmint(2,io) + Ri*SIN(phi)
                    shaper(1,is,il) = RiS*(dT0 + ilT*ddT)
C                    shaper(1,is,il) = 0.
                    shaper(2,is,il) = RiS 
                    shaper(3,is,il) = rmint(3,io) + Ri*COS(phi)
                  ENDDO             ! EndDo over nlR
                ENDDO               ! EndDo over nlT
              ENDDO                 ! EndDo over ndiv
            ELSE
              is = is + 1
              IF(io.EQ.6) THEN
                y0 = rmint(5,io) + ddRr/2.
                z0 = rmint(6,io)
                il = 0
                DO ilT = 1,nlT
                  DO ilR = 1,nlR      
C                  DO ilR = 1,nloopr      
                    il = il + 1
                    RiS = y0 - ilR*ddRr
                    shaper(1,is,il) = RiS*(dT0 + ilT*ddT)
C                    shaper(1,is,il) = 0.
                    shaper(2,is,il) = RiS 
                    shaper(3,is,il) = z0
                  ENDDO 
                ENDDO 
              ENDIF
              IF(io.EQ.7) THEN
                y0 = rmint(5,io) + ddRr/2.
                z0 = rmint(6,io) - ddRr/2. 
                il = 0
                DO ilT = 1,nlT
                  DO ilR = 1,nlR      
C                  DO ilR = 1,nloopr      
                    il = il + 1
                    RiS = y0 - ilR*ddRr
                    shaper(1,is,il) = RiS*(dT0 + ilT*ddT)
C                    shaper(1,is,il) = 0.
                    shaper(2,is,il) = RiS 
                    shaper(3,is,il) = z0 + ilR*ddRr
                  ENDDO 
                ENDDO 
              ENDIF
              IF(io.EQ.8) THEN
                y0 = rmint(5,io) - ddRr/2.
                z0 = rmint(6,io) - ddRr/2.
                il = 0
                DO ilT = 1,nlT
                  DO ilR = 1,nlR      
C                  DO ilR = 1,nlR      
                    il = il + 1
                    RiS = y0 + ilR*ddRr
                    shaper(1,is,il) = RiS*(dT0 + ilT*ddT)
C                    shaper(1,is,il) = 0.
                    shaper(2,is,il) = RiS 
                    shaper(3,is,il) = z0 + ilR*ddRr
                  ENDDO 
                ENDDO 
              ENDIF
            ENDIF                   ! End IF(io .LE. 5)
          ENDDO                     ! End Do over Nox
        ENDIF                       ! End IF(MFM_MAG_type .EQ. 2)
C
      ENDIF                         ! End IF(LFIRST) THEN
C
C
C For any further entry the subroutine begins here
C
C Big Torus
C
C      IF(MFM_MAG_type.EQ.1) THEN
      IF(ipar_trk_magtyp.EQ.1) THEN
C Reset field
        BT(1) = 0.D0
        BT(2) = 0.D0
        BT(3) = 0.D0
C Come back to Cartesian coordinates
        y(1)=X(1)
        y(2)=X(2)*X(1)
        y(3)=X(3)
C Start loop over the individual loops for each coil
        DO i =1,nloops
C Rotate the coil to get position of the other sectors
          DO is = 1,nsect
            DO j = 1,nshape
              SU(1,j) = shape(1,j,i)*COSSEC(is)-shape(2,j,i)*SINSEC(is)
              SU(2,j) = shape(1,j,i)*SINSEC(is)+shape(2,j,i)*COSSEC(is)
              SU(3,j) = shape(3,j,i)
            ENDDO
            DO k = 1,nshape
              m = k + 1
C Close the loop
              IF(m .GT. nshape) m = m - nshape
C And sum the field of all these straight elements
              CALL MFM_TORFIELD(y,SU(1,k),SU(1,m), BT)
            ENDDO
          ENDDO               ! EndDo over sectors
        ENDDO              ! EndDo over loops
C
        F(1) = XNORM1*BT(1)
        F(2) = XNORM1*BT(2)
        F(3) = XNORM1*BT(3)
      ENDIF
C
C Mini Torus
C
C      IF(MFM_MAG_type.EQ.2) THEN
      IF(ipar_trk_magtyp.EQ.2) THEN
C Reset MTorus field
        BT(1) = 0.D0
        BT(2) = 0.D0
        BT(3) = 0.D0
C Come back to Cartesian coordinates
        y(1)=X(1)
        y(2)=X(2)*X(1)
        y(3)=X(3)
C Start loop over the individual loops for each coil
        DO i = 1,nloopr
C Rotate the coil to get position of the others
          DO is = 1,nsect
            DO j = 1,nshaper
              SUr(1,j) = shaper(1,j,i)*COSSEC(is)-shaper(2,j,i)*SINSEC(is)
              SUr(2,j) = shaper(1,j,i)*SINSEC(is)+shaper(2,j,i)*COSSEC(is)
              SUr(3,j) = shaper(3,j,i)
            ENDDO
            DO k = 1,nshaper
              m = k + 1
C Close the loop
              IF(m .GT. nshaper) m = m - nshaper
C Sum the field of all these straight elements
c              WRITE(CRMESS,*)'y=',y,', coil=',is,', cond=',i,', part=',k  
c              CALL RECMES(CRNAME,'I',CRMESS)
              CALL MFM_TORFIELD(y,SUr(1,k),SUr(1,m), BT)
            ENDDO
          ENDDO               ! EndDo over sectors
        ENDDO              ! EndDo over loops
C
        F(1) = XNORM2*BT(1)
        F(2) = XNORM2*BT(2)
        F(3) = XNORM2*BT(3)
      ENDIF
C
C      IF(MFM_MAG_type.EQ.3) THEN
      IF(ipar_trk_magtyp.EQ.3) THEN
C Reset Polarized Target field
        R=ABS(X(1))*SQRT(1.+X(2)**2)
        Z=X(3)
        CALL MFM_PTGFIELD(R,Z,BR,BZ)
C
        IF ( X(1) .NE. 0.) THEN
          F(1) = XNORM3*BR*X(1)/R
          F(2) = F(1)*X(2)
        ELSE
          F(1) = 0.
          F(2) = 0.
        ENDIF
        F(3) = XNORM3*BZ
      ENDIF
C
      RETURN
      END
