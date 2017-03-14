      Subroutine Re_mult()
c
      IMPLICIT NONE
      SAVE
c
#include "remult.inc"
      common/ifint/j,lm,is,ii,w,q2
      integer j,lm,is,ii
      real w,q2 
c ii, W and Q2 are not used here 
c
      if(lm.ge.0)then
         Elp(lm,is)=(-2.*lm*Rint(lm,is,1)+Rint(lm,is,2)+
     +2.*lm*Rint(lm,is,3)-lm*(lm-1)*Rint(lm,is,4))/(2.*(lm+1.)*(2.*lm+1.))
         Slp(lm,is)=(Rint(lm,is,5)-lm*Rint(lm,is,6))/(lm+1.)/(2.*lm+1.)
         Mlp(lm,is)=0.
         Mlm(lm,is)=0.
         Elm(lm,is)=0.
         Slm(lm,is)=0.
      endif
      if(lm.ge.1)then
         Mlp(lm,is)=(2.*Rint(lm,is,1)+Rint(lm,is,2)+2.*lm*Rint(lm,is,3)+
     +(lm-1.)*(lm+2.)*Rint(lm,is,4))/(2.*(lm+1.)*(2.*lm+1.))
         Mlm(lm,is)=(-2.*Rint(lm,is,1)-Rint(lm,is,2)+2.*(lm+1)*Rint(lm,is,3)-
     -(lm-1.)*(lm+2.)*Rint(lm,is,4))/(2.*lm*(2.*lm+1.))
         Slm(lm,is)=(Rint(lm,is,5)+(lm+1.)*Rint(lm,is,6))/(lm*(2.*lm+1.))
      endif
      if(lm.ge.2)then
	Elm(lm,is)=(2.*(lm+1.)*Rint(lm,is,1)+Rint(lm,is,2)-2.*(lm+1.)*Rint(lm,is,3)-
     -(lm+1.)*(lm+2.)*Rint(lm,is,4))/(2.*lm*(2.*lm+1.))
      endif
c
      return
      end
