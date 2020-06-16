
	subroutine lab2cms(acmk
     $	                   ,E0,E_el,th_el,ph_el,W,Q2
     $	                   ,EPI1,PPI1X,PPI1Y,PPI1Z
     $	                   ,EPI2,PPI2X,PPI2Y,PPI2Z
     $	                   ,EN,PNX,PNY,PNZ
     $	                   ,v,m
     $                     ,psi_12)

C*****************************************************************************
C
c       INPUT: pi1 = pi-            pi2 = pi+
c					         
C	Event reconstruction for particle decay  
c       
c       acmk  = 1   2pi chan     hyp       pi1 pi2  -> vector meson    (rho)     , meson pi2=pi+
c       acmk  = 2   2pi chan     hyp    p      pi2  -> Excited baryon  (delta++) , meson pi2=pi+
c       acmk  = 4   2pi chan     hyp    p  pi1      -> Excited baryon  (delta0)  , meson pi1=pi-
c       acmk  = 5   2pi chan     hyp   All 3 chooses above 
c       acmk  = 3   1pi chan            p  pi1     -> Excited baryon (only pi1)
c       el_in = vector of electron variables: e_beam,e_el,th_el,ph_el,w,Q2
c       P_IN1 = 4-momentum of first particle                INPUT
c       P_IN2 = 4-momentum of second  particle              INPUT
c       P_BAR = 4-momentum of barion                        INPUT
c       P_v   = 4-momentum of final particle:               OUTPUT
c               (1) =  p  - pi1
c               (2) = pi1 - pi2
c               (3) =  p  - pi2
c       P_meson  = 4-momentum of pion in vector rest-frame of final part  OUTPUT
c               (4) =  hcm for single pion 
c               (1) =  rest of (p-pi1)
c               (2) =  rest of (pi1-pi2)
c               (3) =  rest of (p-pi2)
C 					         
C*****************************************************************************
	IMPLICIT NONE
c        include "ntpl_goa.inc"

	INTEGER ACMK


	REAL q_M,q(0:3),Q2
	REAL CTH,THETA_Q,th_partic,ph_partic
	REAL P_PARTIC_M,E_PARTIC,P_PARTICX,P_PARTICY,P_PARTICZ

	real v(0:7,3),  m(0:7,4)
	real psi_12(3), sss

	REAL TH_PI11,PH_PI11
	REAL TH_PI12,PH_PI12
	REAL TH_PI13,PH_PI13
	REAL TH_PI14,PH_PI14


	REAL PPI1_M,EPI1,PPI1X,PPI1Y,PPI1Z
	REAL PPI1_M1,EPI11,PPI1X1,PPI1Y1,PPI1Z1
	REAL PPI1_M2,EPI12,PPI1X2,PPI1Y2,PPI1Z2
	REAL PPI1_M3,EPI13,PPI1X3,PPI1Y3,PPI1Z3
	REAL PPI1_M4,EPI14,PPI1X4,PPI1Y4,PPI1Z4

	REAL EPI16,PPI1X6,PPI1Y6,PPI1Z6
	REAL TH_PI21,PH_PI21
	REAL TH_PI22,PH_PI22
	REAL TH_PI23,PH_PI23
	REAL TH_PI24,PH_PI24


	REAL PPI2_M,EPI2,PPI2X,PPI2Y,PPI2Z
	REAL PPI2_M1,EPI21,PPI2X1,PPI2Y1,PPI2Z1
	REAL PPI2_M2,EPI22,PPI2X2,PPI2Y2,PPI2Z2
	REAL PPI2_M3,EPI23,PPI2X3,PPI2Y3,PPI2Z3
	REAL PPI2_M4,EPI24,PPI2X4,PPI2Y4,PPI2Z4


	REAL PN_M,EN,PNX,PNY,PNZ
	REAL TH_N1,PH_N1
	REAL PN_M1,EN1,PNX1,PNY1,PNZ1
	REAL TH_N2,PH_N2
	REAL PN_M2,EN2,PNX2,PNY2,PNZ2
	REAL TH_N3,PH_N3
	REAL PN_M3,EN3,PNX3,PNY3,PNZ3
	REAL TH_N4,PH_N4
	REAL PN_M4,EN4,PNX4,PNY4,PNZ4


	REAL e_el,e0,w,th_el,ph_el,degrad


	LOGICAL flag_phi_prod

        data degrad/0.0174533/


C-------------------------------------
C	Electron kinematics
C-------------------------------------

	q(1) = - E_EL*SIN(degrad*TH_EL)*COS(degrad*PH_EL)
	q(2) = - E_EL*SIN(degrad*TH_EL)*SIN(degrad*PH_EL)
	q(3) = E0 - E_EL*COS(degrad*TH_EL)

	q_M = SQRT(q(1)**2 + q(2)**2 + q(3)**2)

	CTH = q(3)/q_m

	IF((CTH-1).GE.0.AND.(CTH-1).LT.1.E-4) CTH = 1.

	THETA_Q = ACOS(CTH)

	PPI1_M = sqrt(PPI1X**2+PPI1Y**2+PPI1Z**2)
	PPI2_M = sqrt(PPI2X**2+PPI2Y**2+PPI2Z**2)
	PN_M   = sqrt(PNX**2+PNY**2+PNZ**2)






C------------------------------------------------------------------------
C	Rotation around the LAB z axis  
C	(z axis along the beam, x axis on the ee' plane)
C------------------------------------------------------------------------

	PPI1X1 = COS(degrad*PH_EL)*PPI1X + SIN(degrad*PH_EL)*PPI1Y
	PPI1Y1 = - SIN(degrad*PH_EL)*PPI1X + COS(degrad*PH_EL)*PPI1Y
	PPI1Z1 = PPI1Z

	EPI11 = EPI1
	PPI1_M1 = PPI1_M

	CALL ANGLES(0,PPI1X1,PPI1Y1,PPI1Z1,TH_PI11,PH_PI11)

	PPI2X1 = COS(degrad*PH_EL)*PPI2X + SIN(degrad*PH_EL)*PPI2Y
	PPI2Y1 = - SIN(degrad*PH_EL)*PPI2X + COS(degrad*PH_EL)*PPI2Y
	PPI2Z1 = PPI2Z
	PPI2_M = sqrt(PPI2X1**2+PPI2Y1**2+PPI2Z1**2)
	EPI21 = EPI2
	PPI2_M1 = PPI2_M

	CALL ANGLES(0,PPI2X1,PPI2Y1,PPI2Z1,TH_PI21,PH_PI21)

	EN1 = EN
	PN_M1 = PN_M

	PNX1 = COS(degrad*PH_EL)*PNX + SIN(degrad*PH_EL)*PNY
	PNY1 = - SIN(degrad*PH_EL)*PNX + COS(degrad*PH_EL)*PNY
	PNZ1 = PNZ

	CALL ANGLES(0,PNX1,PNY1,PNZ1,TH_N1,PH_N1)

C------------------------------------------------------------------------
C	Rotation in the LAB plane 
C	(z axis along the q vector, x axis on the ee' plane)
C------------------------------------------------------------------------

	CALL 
     @  ROTATION_INV
     @  (0.,W,Q2,E0,PPI1X1,PPI1Y1,PPI1Z1,PPI1X2,PPI1Y2,PPI1Z2)

	EPI12 = EPI11
	PPI1_M2 = PPI1_M1

	CALL ANGLES(0,PPI1X2,PPI1Y2,PPI1Z2,TH_PI12,PH_PI12)

	CALL 
     @  ROTATION_INV
     @  (0.,W,Q2,E0,PPI2X1,PPI2Y1,PPI2Z1,PPI2X2,PPI2Y2,PPI2Z2)

	EPI22 = EPI21
	PPI2_M2 = PPI2_M1

	CALL ANGLES(0,PPI2X2,PPI2Y2,PPI2Z2,TH_PI22,PH_PI22)

	CALL 
     @  ROTATION_INV
     @  (0.,W,Q2,E0,PNX1,PNY1,PNZ1,PNX2,PNY2,PNZ2)

	EN2 = EN1
	PN_M2 = PN_M1

	CALL ANGLES(0,PNX2,PNY2,PNZ2,TH_N2,PH_N2)

C--------------------------------------------------
C	Hypothesis of pi+,pi- forming a rho meson
C--------------------------------------------------

	  E_PARTIC = EPI12 + EPI22
	  P_PARTICX = PPI1X2 + PPI2X2
	  P_PARTICY = PPI1Y2 + PPI2Y2
	  P_PARTICZ = PPI1Z2 + PPI2Z2

	  P_PARTIC_M = SQRT(P_PARTICX**2 + P_PARTICY**2 + P_PARTICZ**2)

	CALL ANGLES(1,P_PARTICX,P_PARTICY,P_PARTICZ,TH_PARTIC,PH_PARTIC)

	IF(TH_PARTIC.LE.0.01) PH_PARTIC = 0.

C---------------------------------------------------------------------------
C	Rotation to the hadronic plane 
C	(z axis still along the vector q but x axis on the hadronic plane)
C---------------------------------------------------------------------------

C+ If you chose flag_phi_prod = .false. you will lose information about production phi
	flag_phi_prod = .true.
	if(flag_phi_prod) then 
	  PPI1X3 = PPI1X2  
	  PPI1Y3 = PPI1y2 
	  PPI1Z3 = PPI1Z2
	  EPI13 = EPI12
	  PPI1_M3 = PPI1_M2
	  TH_PI13 = TH_PI12 
	  PH_PI13 = pH_PI12 

	  PPI2X3 = PPI2X2 
	  PPI2Y3 = PPI2Y2
	  PPI2Z3 = PPI2Z2

	  EPI23 = EPI22
	  PPI2_M3 = PPI2_M2
	  TH_PI23 = TH_PI22 
	  PH_PI23 = pH_PI22 

	  EN3 = EN2
	  PNX3 = PNX2
	  PNY3 = PNY2
	  PNZ3 = PNZ2
	  PN_M3 = PN_M2
	  TH_N3 = TH_N2 
	  PH_N3 = pH_N2
 	else
	  PPI1X3 = COS(PH_PARTIC)*PPI1X2 + SIN(PH_PARTIC)*PPI1Y2
	  PPI1Y3 = - SIN(PH_PARTIC)*PPI1X2 + COS(PH_PARTIC)*PPI1Y2
	  PPI1Z3 = PPI1Z2

	  EPI13 = EPI12
	  PPI1_M3 = PPI1_M2

	CALL ANGLES(0,PPI1X3,PPI1Y3,PPI1Z3,TH_PI13,PH_PI13)

	  PPI2X3 = COS(PH_PARTIC)*PPI2X2 + SIN(PH_PARTIC)*PPI2Y2
	  PPI2Y3 = - SIN(PH_PARTIC)*PPI2X2 + COS(PH_PARTIC)*PPI2Y2
	  PPI2Z3 = PPI2Z2

	  EPI23 = EPI22
	  PPI2_M3 = PPI2_M2

	CALL ANGLES(0,PPI2X3,PPI2Y3,PPI2Z3,TH_PI23,PH_PI23)

	  EN3 = EN2
c	  PN_M3 = PN_M2

	  PNX3 = - PPI1X3 - PPI2X3
	  PNY3 = - PPI1Y3 - PPI2Y3
	  PNZ3 = q_M - PPI1Z3 - PPI2Z3

	  PN_M3 = SQRT(PNX3**2 + PNY3**2 + PNZ3**2)

	 CALL ANGLES(0,PNX3,PNY3,PNZ3,TH_N3,PH_N3)
	endif
C----------------------------------------------------------------------
C	Lorentz boost along the virtual photon momentum to the CMS
C	(z axis along q vector but x axis still on the leptonic plane 
c        if flag_phi_prod = .true.)
C----------------------------------------------------------------------

	CALL 
     @	LORENTZ_INV
     @  (0,W,Q2,EPI13,PPI1X3,PPI1Y3,PPI1Z3,
     @  EPI14,PPI1X4,PPI1Y4,PPI1Z4,TH_PI14,PH_PI14)

	PPI1_M4 = SQRT(PPI1X4**2 + PPI1Y4**2 + PPI1Z4**2)

	CALL 
     @	LORENTZ_INV
     @  (0,W,Q2,EPI23,PPI2X3,PPI2Y3,PPI2Z3,
     @  EPI24,PPI2X4,PPI2Y4,PPI2Z4,TH_PI24,PH_PI24)
	  
	PPI2_M4 = SQRT(PPI2X4**2 + PPI2Y4**2 + PPI2Z4**2)

	CALL 
     @	LORENTZ_INV
     @  (0,W,Q2,EN3,PNX3,PNY3,PNZ3,
     @  EN4,PNX4,PNY4,PNZ4,TH_N4,PH_N4)
	  
	PN_M4 = SQRT(PNX4**2 + PNY4**2 + PNZ4**2)

	CALL ANGLES(0,PNX4,PNY4,PNZ4,TH_N4,PH_N4)

c+	Hypothesis of pi+,pi- forming a rho meson
	if(acmk.eq.1.or.acmk.eq.5) 
     $	     call cm_and_rest
     $                 (EPI14,PPI1X4,PPI1Y4,PPI1Z4
     $	               ,EPI24,PPI2X4,PPI2Y4,PPI2Z4
     $ ,v(0,2),v(1,2),v(2,2),v(3,2),v(4,2),v(5,2),v(6,2),v(7,2)
     $ ,m(0,2),m(1,2),m(2,2),m(3,2),m(4,2),m(5,2),m(6,2),m(7,2))
c     $ ,psi_12(2))
c- Ending Hyp of pi+ pi- in rho
	


C+ Hypothesis of p pi1 - forming an excited baryon  (N*)  -> _PARTIC = N* in CM system
	if(acmk.eq.2.or.acmk.eq.3.or.acmk.eq.5) 
     $	     call cm_and_rest
     $                 (EN4,PNX4,PNY4,PNZ4
     $	               ,EPI14,PPI1X4,PPI1Y4,PPI1Z4
     $ ,v(0,1),v(1,1),v(2,1),v(3,1),v(4,1),v(5,1),v(6,1),v(7,1)
     $ ,m(0,1),m(1,1),m(2,1),m(3,1),m(4,1),m(5,1),m(6,1),m(7,1))
c     $ ,psi_12(1))
	     
c- Ending Hyp of p-pi1 forming a Delta0

C+ Hypothesis of p pi2 - forming an excited baryon  (N*)  -> _PARTIC = N* in CM system
	if(acmk.eq.4.or.acmk.eq.5) 
     $	     call cm_and_rest
     $                 (EN4,PNX4,PNY4,PNZ4
     $	               ,EPI24,PPI2X4,PPI2Y4,PPI2Z4
     $ ,v(0,3),v(1,3),v(2,3),v(3,3),v(4,3),v(5,3),v(6,3),v(7,3)
     $ ,m(0,3),m(1,3),m(2,3),m(3,3),m(4,3),m(5,3),m(6,3),m(7,3))
c     $ ,psi_12(3))

c- Ending Hyp of p-pi2 forming a Deltapp

c+ Angle between mother and decaying particles in LAB frame
c++ rho hyp: psi=rho%pip
       sss = sqrt((ppi1x+ppi2x)**2 +(ppi1y+ppi2y)**2 +(ppi1z+ppi2z)**2)
       sss = sss*sqrt(ppi2x**2+ppi2y**2+ppi2z**2)
       if(sss.ne.0.) then 
       psi_12(2) = acos( ((ppi1x+ppi2x)*ppi2x +(ppi1y+ppi2y)*ppi2y +(ppi1z+ppi2z)*ppi2z)
     $               /sss )/degrad
       else
       psi_12(2) = 0.0
       endif
c++ Dpp hyp: psi=delta%pip
       sss = sqrt((pnx+ppi2x)**2+ (pny+ppi2y)**2 + (pnz+ppi2z)**2)
       sss = sss*sqrt(ppi2x**2+ppi2y**2+ppi2z**2)
       if(sss.ne.0.) then
       psi_12(3) = acos( ((pnx+ppi2x)*ppi2x+ (pny+ppi2y)*ppi2y + (pnz+ppi2z)*ppi2z)
     $               /sss )/degrad
       else
       psi_12(3) = 0.0
       endif
c++ D0 hyp: psi=Delta%pim,
       sss = sqrt((pnx+ppi1x)**2+ (pny+ppi1y)**2 + (pnz+ppi1z)**2)
       sss = sss*sqrt(ppi1x**2+ppi1y**2+ppi1z**2)
       if(sss.ne.0.) then
       psi_12(1) = acos(((pnx+ppi1x)*ppi1x+ (pny+ppi1y)*ppi1y + (pnz+ppi1z)*ppi1z) 
     $               /sss )/degrad
       else
       psi_12(1) = 0.0
       endif

c++ If 1pi channel  skip the transformations to the rest frame
	 if(acmk.eq.3) then	  
	ppi1x6=ppi1x4
        ppi1y6=ppi1y4
        ppi1z6=ppi1z4
        epi16 =epi14
         m(0,4)= EPI16
	 m(1,4)= ppi1x6
	 m(2,4)= PPI1Y6 
	 m(3,4)= PPI1Z6 
	 m(5,4) = sqrt(PPI1X6**2+PPI1Y6**2+PPI1Z6**2)
	 m(4,4) =EPI16**2-(PPI1X6**2+PPI1Y6**2+PPI1Z6**2)
	 call angles(0,m(1,4),m(2,4),m(3,4),m(6,4),m(7,4))
	   endif
c--
c++ If inclusive pion channel do only the first two rotation 
c   and the Lorentz boost
	   if(acmk.eq.9) then
	CALL 
     @	LORENTZ_INV
     @  (0,W,Q2,EPI12,PPI1X2,PPI1Y2,PPI1Z2,
     @  EPI14,PPI1X4,PPI1Y4,PPI1Z4,TH_PI14,PH_PI14)

	PPI1_M4 = SQRT(PPI1X4**2 + PPI1Y4**2 + PPI1Z4**2)

	ppi1x6=ppi1x4
        ppi1y6=ppi1y4
        ppi1z6=ppi1z4
        epi16 =epi14
         m(0,4)= EPI16
	 m(1,4)= ppi1x6
	 m(2,4)= PPI1Y6 
	 m(3,4)= PPI1Z6 
	 m(5,4) = sqrt(PPI1X6**2+PPI1Y6**2+PPI1Z6**2)
	 m(4,4) =EPI16**2-(PPI1X6**2+PPI1Y6**2+PPI1Z6**2)
	 call angles(0,m(1,4),m(2,4),m(3,4),m(6,4),m(7,4))
	   endif
c---

	      
10	 return
	END

