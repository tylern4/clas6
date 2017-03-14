      Function Finthigh(x)

      IMPLICIT NONE
      SAVE
      
#include "rephi.inc"

      common/ifintg/lm,is,i,j,w,q2

      Real Finthigh,x
      Real W,Q2
      Integer i,j,is,lm,ii,ir
      real p0,p1,p2,p3,p4,p5,p6,rim(6,3),PHis(6,3)
      real pim(6,2),pip(6,5,2),PHisotop(6,3)
      real extB,extR
      integer ierr

      call janr_rephi(W,Q2,x,ierr)
      
      extB = 1./(1.+(w**2-1.16)**2)
      extR = extB*(w**2-1.16)**2
      
      do ii=1,6
        do ir=1,3
          Phisotop(ii,ir)=Phisot(1,ii,ir)*extB+Phisot(2,ii,ir)*extR
        enddo
      enddo

c- is=1 for (3/2), is=2 for (1/2), is=3 for (0)

c- (0)
      if(is.eq.3)then
         do ii=1,6
            PHis(ii,is)=Phisotop(ii,3)
         enddo
c- (1/2)
      elseif(is.eq.2)then
         do ii=1,6
            PHis(ii,is)=Phisotop(ii,1)+2.*Phisotop(ii,2)
         enddo
c- (3/2)
      elseif(is.eq.1)then
         do ii=1,6
            PHis(ii,is)=Phisotop(ii,1)-Phisotop(ii,2)
         enddo
      endif

	p0=1.-x**2
c-P_4
	p1=(3.-30.*x**2+35.*x**4)/8.
c-P_5
	p2=(15.*x-70.*x**3+63.*x**5)/8.
c-P'_4
        p3=-7.5*x+17.5*x**3
c-P'_5
        p4=1.875-26.25*x**2+39.375*x**4
c-P''_4
        p5=-7.5+52.5*x**2  
c-P''_5
        p6=-52.5*x+157.5*x**3

      rim(1,is) = PHis(3,is)+x*PHis(4,is)
      rim(2,is) = 2.*PHis(1,is)-2.*x*PHis(2,is)+p0*PHis(4,is)
      rim(3,is) = PHis(2,is)
      rim(4,is) = PHis(4,is)
      rim(5,is) = PHis(5,is)+x*PHis(6,is)
      rim(6,is) = PHis(6,is)

      pim(1,1) = -9.*p0*p3/45.
      pim(1,2) = -11.*p0*p4/60.
      pim(2,1) = 4.5*p1
      pim(2,2) = 5.5*p2
      pim(3,1) = -pim(1,1)
      pim(3,2) = -pim(1,2)
      pim(4,1) =     p0**2*p5/80.
      pim(4,2) = 11.*p0**2*p6/1680.
      pim(5,1) = pim(2,1)
      pim(5,2) = pim(2,2)
      pim(6,1) = pim(3,1)
      pim(6,2) = pim(3,2)
      
      if(lm.eq.5)then
        do ii=1,6
          pip(ii,lm,is)=pim(ii,2)*rim(ii,is)
        enddo
      elseif(lm.eq.4)then  
        do ii=1,6   
          pip(ii,lm,is)=pim(ii,1)*rim(ii,is)
        enddo
      endif  

      finthigh=0.0 
      goto(11,12,13,14,15,16)i

 11   continue
c - Integral 1 (I1)   
      finthigh=pip(1,lm,is)
      go to 113

 12   continue
c - Integral 2 (I2)
      finthigh=pip(2,lm,is)
      go to 113

 13   continue
c - Integral 3 (I3)   
      finthigh=pip(3,lm,is)
      go to 113

 14   continue
c - Integral 4 (I4)
      finthigh=pip(4,lm,is)
      go to 113

 15   continue
c - Integral 5 (I5)
      finthigh=pip(5,lm,is)
      go to 113

 16   continue
c - Integral 6 (I6)
      finthigh=pip(6,lm,is)
      
113   continue

      return
      end

