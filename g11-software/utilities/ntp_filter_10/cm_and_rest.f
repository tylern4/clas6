 	subroutine cm_and_rest(E1,PX1,PY1,PZ1
     $	               ,E2,PX2,PY2,PZ2
     $	               ,Ecm,Pxcm,PYcm,PZcm,W2cm,Pmcm,thcm,phcm
     $	               ,Er,Pxr,Pyr,PZr,W2r,pr,thr,phr)
c     $                 ,psi)

C*****************************************************************************
c       P1 = 4-momentum of first particle                INPUT
c       P2 = 4-momentum of second  particle              INPUT
c       PCM = 4-momentum of (12) in cm                   OUTPUT
c       PR  = 4-momentum of 2 in (12)-rest frame         OUTPUT
C*****************************************************************************
	IMPLICIT NONE
      REAL*4   	        E1,PX1,PY1,PZ1
     $	               ,E2,PX2,PY2,PZ2
     $	               ,Ecm,Pxcm,PYcm,PZcm,W2cm,Pmcm,thcm,phcm
     $	               ,Er,Pxr,Pyr,PZr,w2r,pr,thr,phr,Pxr_lep,Pyr_lep,PZr_lep
     $	               ,Pxr_lep1,Pyr_lep1,PZr_lep1
     $	               ,thcm_rad,phcm_rad,psi

      REAL*4 wcm
      REAL degrad
      data degrad/0.0174533/

      thcm   = -1000
      phcm   = -1000
      thr   = -1000
      phr   = -1000
      
c     psi = acos( (px1*px2+py1*py2+pz1*pz2)
c     $               /sqrt(px1**2+py1**2+pz1**2)
c     $               /sqrt(px2**2+py2**2+pz2**2) )/degrad
      Ecm  = E1  + E2
      Pxcm = PX1 + PX2
      Pycm = Py1 + Py2
      Pzcm = Pz1 + Pz2
      pmcm = sqrt(Pxcm**2+ Pycm**2+ Pzcm**2)
      W2cm = Ecm**2 - (Pxcm**2+ Pycm**2+ Pzcm**2)
      IF(W2cm.GE.0) THEN
	 Wcm = SQRT(W2cm)
      ELSE
	 goto 1010
      ENDIF
      call angles(0,Pxcm,Pycm,Pzcm,thcm,phcm)
      call angles(1,Pxcm,Pycm,Pzcm,thcm_rad,phcm_rad)

c+++ Rotation to the hadronic plane
       Pxr_lep =   COS(phcm_rad)*PX2 + SIN(phcm_rad)*PY2 
       Pyr_lep =  -SIN(phcm_rad)*PX2 + COS(phcm_rad)*PY2
       Pzr_lep =   pz2

	

c+++ Rotation in hadronic CMS(z axis along decaying particle momentum
c+++	                       but x axis still on the hadronic plane)
       Pxr_lep1 =   COS(thcm_rad)*PXr_lep - SIN(thcm_rad)*Pzr_lep 
       Pyr_lep1 =   PYr_lep
       Pzr_lep1 =   SIN(thcm_rad)*PXr_lep + COS(thcm_rad)*Pzr_lep

C-----------------------------------------------------------
C	Lorentz boost to the rest frame of the particle 
C	(z axis still along the decaying particle momentum)
C-----------------------------------------------------------
     
	CALL 
     @  LORENTZ_INV
     @  (3,Ecm,Wcm,E2,pxr_lep1,pyr_lep1,pzr_lep1,
     @   Er,pxr,pyr,pzr,thr,phr)
	     pr = sqrt(pxr**2+ pyr**2+pzr**2)
	     w2r = Er**2 - (pxr**2+ pyr**2+pzr**2)

1010        return
            end
             

