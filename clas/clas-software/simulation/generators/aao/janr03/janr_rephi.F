      SUBROUTINE JANR_RePhi(Wi,Q2i,costhi,ierr)

      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "rephi.inc"

      REAL Wi, Q2i, epsilon, costhi, phidi
      INTEGER ierr

      INTEGER i,j
      REAL sinth,theta,im_phi_save(6,3),qkratio,phi
      REAL kgamma,qpion,k22,q22
      REAL W, costh, phid
      REAL Q2, S

      w		= wi
      q2	= q2i
      costh	= costhi
      phid	= phidi

      theta	= acos(costh)
      theta	= theta*180./pi
      sinth	= sqrt(1.-costh**2)
      phi	= phidi*pi/180.

      s		= w**2
      w2	= 2.*w
      E		= s-mn22
      Egamma	= (E-Q2)/w2
      Epion	= (E+mp22)/w2
      q22	= Epion**2-mp22
      k22	= (w-Egamma)**2-mn22
      qpion	= sqrt(q22)
      qkratio	= qpion*w2/E
      kgamma	= sqrt(k22)
      qk	= qpion*kgamma*costh-Epion*Egamma
      t		= mp22-Q2+2.*qk
      u		= 2.*mn22+mp22-Q2-s-t
      
      CALL etta
      CALL born(s,q2)
      CALL re_phi(s,q2)
      
      ierr = 0
      
      RETURN
      END










