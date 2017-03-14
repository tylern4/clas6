      Function Fintg(x)

      IMPLICIT NONE
      SAVE
#include "rephi.inc"
      common/ifintg/lm,is,i,j,w,q2

      Real Fintg,x
      Real W,Q2
      Integer i,j,is,lm,ii
      real p,p1,p2,rim,PHis(6,3,2)
      integer ierr

      call janr_rephi(W,Q2,x,ierr)

      if(is.eq.3)then
         do ii=1,6
            PHis(ii,is,j)=Phisot(j,ii,3)
         enddo
      elseif(is.eq.2)then
         do ii=1,6
            PHis(ii,is,j)=Phisot(j,ii,1)+2.*Phisot(j,ii,2)
         enddo
      elseif(is.eq.1)then
         do ii=1,6
            PHis(ii,is,j)=Phisot(j,ii,1)-Phisot(j,ii,2)
         enddo
      endif

      goto(21,22,23,24)lm+1
      
 21   continue
      p=1.
      p1=0.
      p2=0.
      go to 111

 22   continue
      p=x
      p1=-sqrt(1.-x**2)
      p2=0.
      go to 111

 23   continue
      p=3.*x**2/2.-0.5
      p1=-3.*x*sqrt(1-x**2)
      p2=3.*(1.-x**2)
      go to 111

 24   continue
      p=(5.*x**2-3.)*x/2.
      p1=-sqrt(1.-x**2)*(15.*x**2-3.)/2.
      p2=15.*x*(1-x**2)

 111  continue
      fintg=0.0
      goto(11,12,13,14,15,16)i

 11   continue
c - Integral 1 (I1)
      if(lm.lt.1)return
      rim=p1*sqrt(1.-x**2)*(PHis(3,is,j)+x*PHis(4,is,j))
      fintg=rim*(2.*lm+1.)/(2.*lm*(lm+1.))
      go to 113

 12   continue 
c - Integral 2 (I2)
      rim=p*(2.*PHis(1,is,j)-2.*x*PHis(2,is,j)+(1.-x**2)*PHis(4,is,j))
      fintg=rim*(2.*lm+1.)/2.
      go to 113

 13   continue
c - Integral 3 (I3)
      if(lm.lt.1)return
      rim=p1*sqrt(1.-x**2)*PHis(2,is,j)
      fintg=-rim*(2.*lm+1.)/(2.*lm*(lm+1.))
      go to 113

 14   continue
c - Integral 4 (I4)
      if(lm.lt.2)return
      rim=p2*(1.-x**2)*PHis(4,is,j)
      fintg=rim*(2.*lm+1.)/(2.*lm*(lm+1.)*(lm-1)*(lm+2))
      go to 113

 15   continue 
c - Integral 5 (I5)
      rim=p*(PHis(5,is,j)+x*PHis(6,is,j))
      fintg=rim*(2.*lm+1.)/2.
      go to 113

 16   continue
c - Integral 6 (I6)
      if(lm.lt.1)return
      rim=p1*sqrt(1.-x**2)*PHis(6,is,j)
      fintg=-rim*(2.*lm+1.)/(2.*lm*(lm+1.))

113   continue
      return
      end
