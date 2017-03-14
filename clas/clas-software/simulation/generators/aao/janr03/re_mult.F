      Subroutine Re_mult()

      IMPLICIT NONE
      SAVE

#include "remult.inc"

      common/ifintg/lm,is,ii,jj,w,q2
      integer lm,is,ii,jj
      real w,q2 
      
c ii, W and Q2 are not used here 

      if(lm.ge.0)then
         Elp(lm,is,jj)=(-2.*lm*Rint(lm,is,1,jj)+Rint(lm,is,2,jj)+
     +2.*lm*Rint(lm,is,3,jj)-
     -lm*(lm-1)*Rint(lm,is,4,jj))/(2.*(lm+1.)*(2.*lm+1.))
         Slp(lm,is,jj)=(Rint(lm,is,5,jj)-
     -lm*Rint(lm,is,6,jj))/(lm+1.)/(2.*lm+1.)
         Mlp(lm,is,jj)=0.
         Mlm(lm,is,jj)=0.
         Elm(lm,is,jj)=0.
         Slm(lm,is,jj)=0.
      endif
      
      if(lm.ge.1)then
         Mlp(lm,is,jj)=(2.*Rint(lm,is,1,jj)+
     +Rint(lm,is,2,jj)+2.*lm*Rint(lm,is,3,jj)+
     +(lm-1.)*(lm+2.)*Rint(lm,is,4,jj))/(2.*(lm+1.)*(2.*lm+1.))
         Mlm(lm,is,jj)=(-2.*Rint(lm,is,1,jj)-
     - Rint(lm,is,2,jj)+2.*(lm+1)*Rint(lm,is,3,jj)-
     -(lm-1.)*(lm+2.)*Rint(lm,is,4,jj))/(2.*lm*(2.*lm+1.))
         Slm(lm,is,jj)=(Rint(lm,is,5,jj)+
     -(lm+1.)*Rint(lm,is,6,jj))/(lm*(2.*lm+1.))
      endif
      if(lm.ge.2)then
	Elm(lm,is,jj)=(2.*(lm+1.)*Rint(lm,is,1,jj)+Rint(lm,is,2,jj)-
     -2.*(lm+1.)*Rint(lm,is,3,jj)-
     -(lm+1.)*(lm+2.)*Rint(lm,is,4,jj))/(2.*lm*(2.*lm+1.))
      endif

      return
      end
