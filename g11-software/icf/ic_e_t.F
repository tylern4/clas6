      Subroutine ic_e_t(ierr)
c     
      IMPLICIT NONE
      SAVE      
#include "icraw.inc"
#include "ichit.inc"
#include "iccalib.inc"
#include "ictcl.inc"
      Integer ix,iy,ierr,i,j
      real ehit,noise,rndm
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
                  if(abs(xpos(ix,iy)).lt.10.and.abs(ypos(ix,iy)).lt.10)then
                     nhit=nhit+1
                     eicm(nhit)=ehit
                     ntime(nhit)=ntdc(i)
                     do j=1,ntdc(i)
                        ticm(nhit,j)=tdc(i,j)*tchan(ix,iy)
                     enddo
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





