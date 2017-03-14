      Subroutine ic_e_t(ierr)
c     
      IMPLICIT NONE
      SAVE      
#include "icraw.inc"
#include "ichit.inc"
#include "iccalib.inc"
#include "ictcl.inc"
#include "ic.inc"
      Integer ix,iy,ierr,i,j
      real ehit,noise,rndm,tim
      external rndm
c    
      nhit=0
      Do i=1,ierr
         If(istat(i).eq.-101)Then
c     Generate a rundom noise from 8 to 10 MeV
               ix=ixm(i)
               iy=iym(i)
               ehit=(adc(i)-pedestal(ix,iy))*gain(ix,iy)
               if(ehit.gt.ehitmin.and.nhit.lt.maxhits)then
                  if(abs(xpos(ix,iy)).lt.ic_xpos_max.and.abs(ypos(ix,iy)).lt.ic_ypos_max)then
                     nhit=nhit+1
                     eicm(nhit)=ehit
                     ntime(nhit)=ntdc(i)
                     if(ntdc(i).gt.0)then
                        ticm(nhit) = IC_TDC0(ix,iy)+
     +                       IC_TDC1(ix,iy)/(adc(i)-pedestal(ix,iy)-
     +                       IC_TDC2(ix,iy))-tdc(i,tdcmin(i))*tchan(ix,iy)
                        if(tdcmin2(i).gt.0)then
                           tim = IC_TDC0(ix,iy)+
     +                          IC_TDC1(ix,iy)/(adc(i)-pedestal(ix,iy)-
     +                          IC_TDC2(ix,iy))-tdc(i,tdcmin2(i))*tchan(ix,iy)
                           NextHit(nhit)=ticm(nhit)-tim
                        endif
                     else
                        ticm(i)=-9999.
                        NextHit(i)=-9999.
                     endif
c     print *,nhit,ix,iy,eicm(nhit),ticm(nhit,1)
c                     do j=1,ntdc(i)
c                        ticm(nhit,j)=tdc(i,j)
c                     enddo
                     ClosestHit(nhit) = NextHit(i)
                     xhp(nhit)=xpos(ix,iy)
                     yhp(nhit)=ypos(ix,iy)
                     zhp(nhit)=zpos(ix,iy)
                     ixhit(nhit)=ixm(i)
                     iyhit(nhit)=iym(i)
                     hitstat(nhit)=-nhit
                  endif
               else
                  istat(i)=0
               endif
         EndIf
      EndDo
      call hcdir('//PAWC',' ')
c     
      ierr=nhit
c     
      return
      end





