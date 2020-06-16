C***************************ROTATION_INV*********************************C
C------------------------------------------------------------------------------C
C	Rotates a vector from the frame with the z axis along the  	       C
C	direction of the virtual photon to the frame in which the z axis is    C
C	along the direction of the electron beam			       C
C									       C
C	The subroutine accepts the value of W and Q^2 of the reaction and      C
C	the vector to rotate V(I), and returns the rotated vector VPRIME(I)    C
C------------------------------------------------------------------------------C
C******************************************************************************C



C*******************************************************************************
	SUBROUTINE ROTATION_INV
     @  (PHI_E1,W1,Q2,EB,V1,V2,V3,VPRIME1,VPRIME2,VPRIME3)
C*******************************************************************************

	REAL V(3),VPRIME(3),DROT(3,3)
	REAL V1,V2,V3,VPRIME1,VPRIME2,VPRIME3
	REAL THETA_E,PHI_E1,THETA_Q,PHI_Q
	REAL OMEGA,W1,Q2,MP,EPRIME,EB
	REAL q(3),q_m,PI,CTH,sinus

	PARAMETER(MP=.938,PI=3.1416)

C***************************************************************
C	Evaluation of some quantities used in the calculation
C***************************************************************

	OMEGA = (W1**2 + Q2 - MP**2)/2./MP

	q_m = SQRT(OMEGA**2 + Q2)

	EPRIME = EB - OMEGA

	if(EPRIME.lt.1e-3) then
	  THETA_E = 0.	
	else 
	  sinus = SQRT(Q2/4./EB/EPRIME)
	  if (sinus.gt.1) sinus=1.
	  THETA_E = 2*ASIN(sinus)
	endif
C************************************************************
C	Components of q in the frame of the beam direction
C***********************************************************

	q(1) = - EPRIME*SIN(THETA_E)*COS(PHI_E1)
	q(2) = - EPRIME*SIN(THETA_E)*SIN(PHI_E1)
	q(3) = EB - EPRIME*COS(THETA_E)

C***********************************************
C	Evaluation of theta_q and phi_q
C***********************************************

	CTH = q(3)/q_m

	IF((CTH-1).GE.0.AND.(CTH-1).LT.1.E-4) CTH = 1.

	THETA_Q = ACOS(CTH)

	PHI_Q = PHI_E1 + PI

C**************************************************************
C	Evaluation of the components of the rotation matrix
C**************************************************************

	DROT(1,1) = - COS(THETA_Q)*COS(PHI_Q)
	DROT(1,2) = - SIN(PHI_Q)
	DROT(1,3) =  - SIN(THETA_Q)*COS(PHI_Q)

	DROT(2,1) = COS(THETA_Q)*SIN(PHI_Q)
	DROT(2,2) =  - COS(PHI_Q)
	DROT(2,3) = SIN(THETA_Q)*SIN(PHI_Q)

	DROT(3,1) =  - SIN(THETA_Q)
	DROT(3,2) = 0.
	DROT(3,3) = COS(THETA_Q)


	DO L = 1,3
	 DO K = 1,3
	IF(ABS(DROT(L,K)-1.).LT.1.E-6) DROT(L,K) = 1.
	IF(ABS(DROT(L,K)+1.).LT.1.E-6) DROT(L,K) = -1.
	IF(ABS(DROT(L,K)).LT.1.E-4) DROT(L,K) = 0.
	 ENDDO
	ENDDO

C---------------------------------------------------------
C	Fills the vector to be rotated with the input value
C---------------------------------------------------------

	V(1) = V1
	V(2) = V2
	V(3) = V3

C**************************************************************
C	Evaluation of the components of the rotated vector
C**************************************************************

	DO L = 1,3
	VPRIME(L) = 0.
	 DO K = 1,3

	 VPRIME(L) = VPRIME(L) + V(K)*DROT(L,K)

	 ENDDO
	ENDDO

C--------------------------------------------------------
C	Fills the output values with the rotated vector
C--------------------------------------------------------

	VPRIME1 = VPRIME(1)
	VPRIME2 = VPRIME(2)
	VPRIME3 = VPRIME(3)

	RETURN

	END
