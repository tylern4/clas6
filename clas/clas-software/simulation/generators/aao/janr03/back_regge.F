      SUBROUTINE BACK_REGGE(S,Q2)

      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "multampl.inc"
#include "fit_par.inc"
#include "back_ampl.inc"

      REAL S, Q2

      INTEGER i,j,kpower,iw,jw,minp,maxp,mwp
      data kpower/8/
      REAL wpoint,tmult(8),wx(8),a_point
      REAL bem(4,3),bep(4,3),bmm(4,3),bmp(4,3),bsm(4,3),bsp(4,3)

      wpoint=sqrt(s)
      mwp=1
      do iw=1,maxmpoints-1
         if(wpoint.lt.wtab(iw).and.mwp.eq.1)then
            mwp=iw
            go to 101
         endif
      enddo
 101  minp=mwp-3
      if(minp.le.0)then
         minp=1
         maxp=8
      else
         maxp=mwp+4
         if(maxp.gt.maxmpoints)then
            maxp=maxmpoints
            minp=maxmpoints-7
         endif
      endif
c
      do i=0,3
         do j=1,3
c E"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=raem(iw,i+1,j)
               wx(jw)=wtab(iw)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bem(i+1,j)=a_point
c E"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=raep(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bep(i+1,j)=a_point
c
c M"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=ramm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmm(i+1,j)=a_point
c M"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=ramp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmp(i+1,j)=a_point
c S"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=rasm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsm(i+1,j)=a_point
c S"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=rasp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsp(i+1,j)=a_point
c
         enddo
      enddo
C-constract background multipole amplitudes with I=3/2
	RM3(1)=bmp(2,3)
	RM3(2)=bmm(3,3)
	RM3(3)=bmp(4,3)
	RM3(4)=0.
	RM3(5)=0.
	RM3(6)=bmm(4,3)
	RM3(7)=bmp(3,3)
	RM3(8)=bmm(2,3)
c
	RE3(1)=bep(2,3)
	RE3(2)=bem(3,3)
	RE3(3)=bep(4,3)
	RE3(4)=0.
	RE3(5)=bep(1,3)
	RE3(6)=bem(4,3)
	RE3(7)=bep(3,3)
	RE3(8)=bem(2,3)
c
	RS3(1)=bsp(2,3)
	RS3(2)=bsm(3,3)
	RS3(3)=bsp(4,3)
	RS3(4)=0.
	RS3(5)=bsp(1,3)
	RS3(6)=bsm(4,3)
	RS3(7)=bsp(3,3)
	RS3(8)=bsm(2,3)
C-constract background multipole amplitudes with I=1/2
C-for combinations (1/2) and (0)
	RM1(1,1)=bmm(2,2)
	RM1(1,2)=0.
	RM1(1,3)=bmm(3,2)
	RM1(1,4)=0.
	RM1(1,5)=0.
	RM1(1,6)=bmm(4,2)
	RM1(1,7)=bmp(2,2)
	RM1(1,8)=bmp(4,2)
	RM1(1,9)=bmp(3,2)
c
	RM1(2,1)=bmm(2,1)
	RM1(2,2)=0.
	RM1(2,3)=bmm(3,1)
	RM1(2,4)=0.
	RM1(2,5)=0.
	RM1(2,6)=bmm(4,1)
	RM1(2,7)=bmp(2,1)
	RM1(2,8)=bmp(4,1)
	RM1(2,9)=bmp(3,1)
c
	RE1(1,1)=bem(2,2)
	RE1(1,2)=bep(1,2)
	RE1(1,3)=bem(3,2)
	RE1(1,4)=0.
	RE1(1,5)=0.
	RE1(1,6)=bem(4,2)
	RE1(1,7)=bep(2,2)
	RE1(1,8)=bep(4,2)
	RE1(1,9)=bep(3,2)
c
	RE1(2,1)=bem(2,1)
	RE1(2,2)=bep(1,1)
	RE1(2,3)=bem(3,1)
	RE1(2,4)=0.
	RE1(2,5)=0.
	RE1(2,6)=bem(4,1)
	RE1(2,7)=bep(2,1)
	RE1(2,8)=bep(4,1)
	RE1(2,9)=bep(3,1)
c
	RS1(1,1)=bsm(2,2)
	RS1(1,2)=bsp(1,2)
	RS1(1,3)=bsm(3,2)
	RS1(1,4)=0.
	RS1(1,5)=0.
	RS1(1,6)=bsm(4,2)
	RS1(1,7)=bsp(2,2)
	RS1(1,8)=bsp(4,2)
	RS1(1,9)=bsp(3,2)
c
	RS1(2,1)=bsm(2,1)
	RS1(2,2)=bsp(1,1)
	RS1(2,3)=bsm(3,1)
	RS1(2,4)=0.
	RS1(2,5)=0.
	RS1(2,6)=bsm(4,1)
	RS1(2,7)=bsp(2,1)
	RS1(2,8)=bsp(4,1)
	RS1(2,9)=bsp(3,1)
C-construct background multipole amplitudes with I=1/2
C-for combinations p(1/2) and n(1/2)
	do j=1,9
	RM1(1,j)=RM1(1,j)/3.+RM1(2,j)
	RM1(2,j)=2.*RM1(2,j)-RM1(1,j)
	RE1(1,j)=RE1(1,j)/3.+RE1(2,j)
	RE1(2,j)=2.*RE1(2,j)-RE1(1,j)
	RS1(1,j)=RS1(1,j)/3.+RS1(2,j)
	RS1(2,j)=2.*RS1(2,j)-RS1(1,j)
	enddo

      RETURN
      END

