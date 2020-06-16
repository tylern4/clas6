C***************************LORENTZ_INV**********************************C
C------------------------------------------------------------------------------C
C	This subroutine makes the Lorentz's transformations of the             C
C	vector V to the vector V'					       C
C------------------------------------------------------------------------------C
C******************************************************************************C




C*******************************************************************************
	SUBROUTINE LORENTZ_INV(IFLAG,W,WPARTIC,V0,V1,V2,V3,
     @			         VPRIME0,VPRIME1,VPRIME2,VPRIME3,THETA,PHI)
C*******************************************************************************

	REAL GAMMA_C,BETA_C
	REAL V0,V1,V2,V3,VPRIME0,VPRIME1,VPRIME2,VPRIME3
	REAL VPRIME_M2,VPRIME_M
	REAL THETA,PHI,PHP1
	REAL OMEGA
	REAL W,WPARTIC,EPARTIC,PPARTIC_M
	REAL MP



C-------------------------------------------------------------------------
	PARAMETER(MP=.93827)
C-------------------------------------------------------------------------

C------------------------------------------
C	Evaluation of the momentum of
C	the photon in the new frame
c------------------------------------------

	IF(IFLAG.EQ.0) THEN
	OMEGA = (W**2 + WPARTIC - MP**2)/2./MP
	EPARTIC = MP + OMEGA
	ELSE
	EPARTIC = (W**2 + WPARTIC**2 - MP**2)/2./W
	ENDIF	

	IF(IFLAG.EQ.0) THEN
	PPARTIC_M = SQRT(OMEGA**2 + WPARTIC)
	ELSE
	 IF((EPARTIC**2 - WPARTIC**2).GE.0) THEN
	 PPARTIC_M = SQRT(EPARTIC**2 - WPARTIC**2)
	 ELSE
	 PPARTIC_M = 0
	 ENDIF
	ENDIF

	IF(IFLAG.EQ.3) THEN	
	 EPARTIC = W
	 PPARTIC_M = SQRT(EPARTIC**2 - WPARTIC**2)
	endif
C---------------------------------------------------------
C	Calculation of the Lorentz transformation 
C	from the old frame to the new frame
C---------------------------------------------------------

	BETA_C = PPARTIC_M/EPARTIC
	GAMMA_C = 1/SQRT(1 - BETA_C**2)

	VPRIME0 = GAMMA_C*(V0 - BETA_C*V3)
	VPRIME1 = V1
	VPRIME2 = V2
	VPRIME3 = GAMMA_C*(V3 - BETA_C*V0)

C-----------------------------------------
C	Calculation of the modulus and
C	angles of scattering in the
C	new frame
C-----------------------------------------
 
	VPRIME_M2 = VPRIME1**2 + VPRIME2**2 + VPRIME3**2

	VPRIME_M = SQRT(VPRIME_M2)

C-------------------------------
C	Angles in the new frame
C-------------------------------

	IF(VPRIME_M.NE.0) THEN

	  THETA = ACOS(VPRIME3/VPRIME_M)*180./3.14159265

	  IF (VPRIME1.EQ.0.AND.VPRIME2.EQ.0) PHI = 0

	  IF (VPRIME1.GT.0.AND.VPRIME2.GE.0) THEN
	  PHP1 = VPRIME2/VPRIME1
	  PHI = ATAN(PHP1)*180./3.14159265
	  ENDIF

	  IF (VPRIME1.EQ.0.AND.VPRIME2.GT.0) PHI = 90.

	  IF (VPRIME1.LT.0.AND.VPRIME2.GE.0) THEN
	  PHP1 = VPRIME2/VPRIME1
	  PHI = ATAN(PHP1)*180./3.14159265 + 180.
	  ENDIF
	
	  IF (VPRIME1.LT.0.AND.VPRIME2.LT.0) THEN
	  PHP1 = VPRIME2/VPRIME1
	  PHI = ATAN(PHP1)*180./3.14159265 + 180.
	  ENDIF

	  IF (VPRIME1.EQ.0.AND.VPRIME2.LT.0) PHI = 270.

	  IF (VPRIME1.GT.0.AND.VPRIME2.LT.0) THEN
	  PHP1 = VPRIME2/VPRIME1
	  PHI = ATAN(PHP1)*180./3.14159265 + 360.
	  ENDIF
	
	ELSE

	  THETA = 0
	  PHI = 0
	
	ENDIF

C-------------------------------------------------------------------------------


	RETURN

	END
