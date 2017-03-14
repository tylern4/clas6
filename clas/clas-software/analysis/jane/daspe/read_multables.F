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
      real w,e,m23,m12,ms
      integer i
c
      open(unit=71,file="re0p",status="old")
      open(unit=72,file="rs0p",status="old")
      open(unit=73,file="rs1m",status="old")
      open(unit=74,file="rm1m",status="old")
      do i=1,2
         read(71,*)
         read(72,*)
         read(73,*)
         read(74,*)
      enddo
      do i=1,maxpoints
         read(71,*,end=101)w,e,m23,m12,ms
         ae01(i)=m12
         ae03(i)=m23
         read(72,*,end=101)w,e,m23,m12,ms
         as01(i)=m12
         as03(i)=m23
         read(73,*,end=101)w,e,m23,m12,ms
         as13(i)=m23
         read(74,*,end=101)w,e,m23,m12,ms
         wtab(i)=w
         am13(i)=m23
      enddo
 101  close(71)
      close(72)
      close(73)
      close(74)
c
      return
      end
