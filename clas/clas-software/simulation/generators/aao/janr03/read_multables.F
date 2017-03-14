      Subroutine read_multables
c
c - Read pregenerated tables for multipole amplitudes. 
c     March 2000
c
      IMPLICIT NONE
      SAVE
c
c
#include "multampl.inc"
c
      integer ifile, lun
      character*10 fnamr(24)
      data fnamr/'rm0p.dat','rm0m.dat','re0p.dat','re0m.dat','rs0p.dat',
     +'rs0m.dat',
     +'rm1p.dat','rm1m.dat','re1p.dat','re1m.dat','rs1p.dat','rs1m.dat',
     +'rm2p.dat','rm2m.dat','re2p.dat','re2m.dat','rs2p.dat','rs2m.dat',
     +'rm3p.dat','rm3m.dat','re3p.dat','re3m.dat','rs3p.dat','rs3m.dat'/
c
      real w,e,m23,m12,ms
      integer i,j,iw,ii
c
      do i=0,3
         do j=1,6
            lun=(i+3)*10+j
            ifile=i*6+j
            open(unit=lun,file='multipols/'//fnamr(ifile),status="old")
            do ii=1,2
               read(lun,*)
            enddo
         enddo
c
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
c
      return
      end
