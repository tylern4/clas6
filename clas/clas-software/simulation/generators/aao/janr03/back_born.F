      SUBROUTINE BACK_BORN(S,Q2)

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
               tmult(jw)=aem(iw,i+1,j)
               wx(jw)=wtab(iw)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bem(i+1,j)=a_point
c E"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=aep(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bep(i+1,j)=a_point
c
c M"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=amm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmm(i+1,j)=a_point
c M"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=amp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmp(i+1,j)=a_point
c S"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=asm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsm(i+1,j)=a_point
c S"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=asp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsp(i+1,j)=a_point
c
         enddo
      enddo
      CALL mixing_ampl(S,Q2)
C-constract background multipole amplitudes with I=3/2
	BbM3(1)=bmp(2,3)
	BbM3(2)=bmm(3,3)
	BbM3(3)=bmp(4,3)
	BbM3(4)=0.
	BbM3(5)=0.
	BbM3(6)=bmm(4,3)
	BbM3(7)=bmp(3,3)
	BbM3(8)=bmm(2,3)+M1(1)
c
	BbE3(1)=bep(2,3)
	BbE3(2)=bem(3,3)
	BbE3(3)=bep(4,3)
	BbE3(4)=0.
	BbE3(5)=bep(1,3)+E0(1)
	BbE3(6)=bem(4,3)
	BbE3(7)=bep(3,3)
	BbE3(8)=bem(2,3)
c
	BbS3(1)=bsp(2,3)
	BbS3(2)=bsm(3,3)
	BbS3(3)=bsp(4,3)
	BbS3(4)=0.
	BbS3(5)=bsp(1,3)+S0(1)
	BbS3(6)=bsm(4,3)
	BbS3(7)=bsp(3,3)
	BbS3(8)=bsm(2,3)+S1(1)
C-constract background multipole amplitudes with I=1/2
C-for combinations (1/2) and (0)
	BbM1(1,1)=bmm(2,2)+M1(1)
	BbM1(1,2)=0.
	BbM1(1,3)=bmm(3,2)
	BbM1(1,4)=0.
	BbM1(1,5)=0.
	BbM1(1,6)=bmm(4,2)
	BbM1(1,7)=bmp(2,2)
	BbM1(1,8)=bmp(4,2)
	BbM1(1,9)=bmp(3,2)
c
	BbM1(2,1)=bmm(2,1)+M1(2)
	BbM1(2,2)=0.
	BbM1(2,3)=bmm(3,1)
	BbM1(2,4)=0.
	BbM1(2,5)=0.
	BbM1(2,6)=bmm(4,1)
	BbM1(2,7)=bmp(2,1)
	BbM1(2,8)=bmp(4,1)
	BbM1(2,9)=bmp(3,1)
c
	BbE1(1,1)=bem(2,2)
	BbE1(1,2)=bep(1,2)+E0(1)
	BbE1(1,3)=bem(3,2)
	BbE1(1,4)=0.
	BbE1(1,5)=0.
	BbE1(1,6)=bem(4,2)
	BbE1(1,7)=bep(2,2)
	BbE1(1,8)=bep(4,2)
	BbE1(1,9)=bep(3,2)
c
	BbE1(2,1)=bem(2,1)
	BbE1(2,2)=bep(1,1)+E0(2)
	BbE1(2,3)=bem(3,1)
	BbE1(2,4)=0.
	BbE1(2,5)=0.
	BbE1(2,6)=bem(4,1)
	BbE1(2,7)=bep(2,1)
	BbE1(2,8)=bep(4,1)
	BbE1(2,9)=bep(3,1)
c
	BbS1(1,1)=bsm(2,2)+S1(1)
	BbS1(1,2)=bsp(1,2)+S0(1)
	BbS1(1,3)=bsm(3,2)
	BbS1(1,4)=0.
	BbS1(1,5)=0.
	BbS1(1,6)=bsm(4,2)
	BbS1(1,7)=bsp(2,2)
	BbS1(1,8)=bsp(4,2)
	BbS1(1,9)=bsp(3,2)
c
	BbS1(2,1)=bsm(2,1)+S1(2)
	BbS1(2,2)=bsp(1,1)+S0(2)
	BbS1(2,3)=bsm(3,1)
	BbS1(2,4)=0.
	BbS1(2,5)=0.
	BbS1(2,6)=bsm(4,1)
	BbS1(2,7)=bsp(2,1)
	BbS1(2,8)=bsp(4,1)
	BbS1(2,9)=bsp(3,1)
C-construct background multipole amplitudes with I=1/2
C-for combinations p(1/2) and n(1/2)
	do j=1,9
	BbM1(1,j)=BbM1(1,j)/3.+BbM1(2,j)
	BbM1(2,j)=2.*BbM1(2,j)-BbM1(1,j)
	BbE1(1,j)=BbE1(1,j)/3.+BbE1(2,j)
	BbE1(2,j)=2.*BbE1(2,j)-BbE1(1,j)
	BbS1(1,j)=BbS1(1,j)/3.+BbS1(2,j)
	BbS1(2,j)=2.*BbS1(2,j)-BbS1(1,j)
	enddo


      RETURN
      END

