      Subroutine regge_multables
c
c - Read pregenerated tables for multipole amplitudes. 
c     May 2002
c
      IMPLICIT NONE
      SAVE
c
c
#include "multampl.inc"
c
      integer ifile, lun
      character*10 fnamrr(24)
      data fnamrr/'rrm0p.dat','rrm0m.dat',
     +'rre0p.dat','rre0m.dat','rrs0p.dat',
     +'rrs0m.dat',
     +'rrm1p.dat','rrm1m.dat','rre1p.dat',
     +'rre1m.dat','rrs1p.dat','rrs1m.dat',
     +'rrm2p.dat','rrm2m.dat','rre2p.dat',
     +'rre2m.dat','rrs2p.dat','rrs2m.dat',
     +'rrm3p.dat','rrm3m.dat','rre3p.dat',
     +'rre3m.dat','rrs3p.dat','rrs3m.dat'/
c
      real w,e,m23,m12,ms
      integer i,j,iw,ii
c
      do i=0,3
         do j=1,6
            lun=(i+3)*10+j
            ifile=i*6+j
            open(unit=lun,file='multipols/'//fnamrr(ifile),status="old")
            do ii=1,2
               read(lun,*)
            enddo
         enddo
c
         lun=(i+3)*10
         do iw=1,maxmpoints
c Read rm"L"p.dat
            read(lun+1,*,end=101)w,e,m23,m12,ms
            ramp(iw,i+1,1)=ms
            ramp(iw,i+1,2)=m12
            ramp(iw,i+1,3)=m23
c Read rm"L"m.dat
            read(lun+2,*,end=101)w,e,m23,m12,ms
            ramm(iw,i+1,1)=ms
            ramm(iw,i+1,2)=m12
            ramm(iw,i+1,3)=m23
c Read re"L"p.dat
            read(lun+3,*,end=101)w,e,m23,m12,ms
            raep(iw,i+1,1)=ms
            raep(iw,i+1,2)=m12
            raep(iw,i+1,3)=m23
c Read re"L"m.dat
            read(lun+4,*,end=101)w,e,m23,m12,ms
            raem(iw,i+1,1)=ms
            raem(iw,i+1,2)=m12
            raem(iw,i+1,3)=m23
c Read rs"L"p.dat
            read(lun+5,*,end=101)w,e,m23,m12,ms
            rasp(iw,i+1,1)=ms
            rasp(iw,i+1,2)=m12
            rasp(iw,i+1,3)=m23
c Read rs"L"m.dat
            read(lun+6,*,end=101)w,e,m23,m12,ms
            rasm(iw,i+1,1)=ms
            rasm(iw,i+1,2)=m12
            rasm(iw,i+1,3)=m23
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
