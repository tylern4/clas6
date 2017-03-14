      Subroutine high_multables

c - Read pregenerated tables for multipole amplitudes. 
c     March 2000

      IMPLICIT NONE
      SAVE

#include "multampl.inc"

      integer ifile, lun
      character*10 fnamhigh(12)
      data fnamhigh/'rm4p.dat','rm4m.dat','re4p.dat','re4m.dat','rs4p.dat','rs4m.dat',
     +'rm5p.dat','rm5m.dat','re5p.dat','re5m.dat','rs5p.dat','rs5m.dat'/

      real w,e,m23,m12,ms
      integer i,j,iw,ii

      do i=4,5
         do j=1,6
            lun=(i+3)*10+j
            ifile=(i-4)*6+j
            open(unit=lun,file='multipols/'//fnamhigh(ifile),status="old")
            do ii=1,2
               read(lun,*)
            enddo
         enddo

         lun=(i+3)*10
         do iw=1,maxmpoints
c Read rm"L"p.dat
            read(lun+1,*,end=101)w,e,m23,m12,ms
            amp(iw,i+1,1)=ms
            amp(iw,i+1,2)=m12
            amp(iw,i+1,3)=m23
c Read rm"L"m.dat
            read(lun+2,*,end=101)w,e,m23,m12,ms
            amm(iw,i+1,1)=ms
            amm(iw,i+1,2)=m12
            amm(iw,i+1,3)=m23
c Read re"L"p.dat
            read(lun+3,*,end=101)w,e,m23,m12,ms
            aep(iw,i+1,1)=ms
            aep(iw,i+1,2)=m12
            aep(iw,i+1,3)=m23
c Read re"L"m.dat
            read(lun+4,*,end=101)w,e,m23,m12,ms
            aem(iw,i+1,1)=ms
            aem(iw,i+1,2)=m12
            aem(iw,i+1,3)=m23
c Read rs"L"p.dat
            read(lun+5,*,end=101)w,e,m23,m12,ms
            asp(iw,i+1,1)=ms
            asp(iw,i+1,2)=m12
            asp(iw,i+1,3)=m23
c Read rs"L"m.dat
            read(lun+6,*,end=101)w,e,m23,m12,ms
            asm(iw,i+1,1)=ms
            asm(iw,i+1,2)=m12
            asm(iw,i+1,3)=m23
            wtab(iw)=w
         enddo
 101     continue
         do j=1,6
            lun=(i+3)*10+j
            close(lun)
         enddo
      enddo

      return
      end
