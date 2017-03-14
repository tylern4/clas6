      SUBROUTINE BACK_HIGH(S)

      IMPLICIT NONE
      SAVE
      
#include "gpar.inc"
#include "multampl.inc"
#include "fit_par.inc"
#include "highmult.inc"

      REAL S

      INTEGER i,j,kpower,iw,jw,minp,maxp,mwp
      data kpower/8/
      REAL wpoint,tmult(8),wx(8),a_point
      REAL bem(2,3),bep(2,3),bmm(2,3),bmp(2,3),bsm(2,3),bsp(2,3)

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

      do i=4,5
         do j=1,3
c E"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=aem(iw,i+1,j)
               wx(jw)=wtab(iw)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bem(i-3,j)=a_point
c E"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=aep(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bep(i-3,j)=a_point
c
c M"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=amm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmm(i-3,j)=a_point
c M"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=amp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bmp(i-3,j)=a_point
c S"l"m
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=asm(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsm(i-3,j)=a_point
c S"l"p
            jw=0
            do iw=minp,maxp
               jw=jw+1
               tmult(jw)=asp(iw,i+1,j)
            enddo
            call polint(tmult,wx,kpower,wpoint,a_point)
            bsp(i-3,j)=a_point
c
         enddo
      enddo
      
C-construct background multipole amplitudes with I=1/2
C-for combinations p(1/2) and n(1/2)

	E4m(1) =  bem(1,2)/3.+bem(1,1)
	E4m(2) = -bem(1,2)/3.+bem(1,1)
	E4p(1) =  bep(1,2)/3.+bep(1,1)
	E4p(2) = -bep(1,2)/3.+bep(1,1)
	M4m(1) =  bmm(1,2)/3.+bmm(1,1)
	M4m(2) = -bmm(1,2)/3.+bmm(1,1)
	M4p(1) =  bmp(1,2)/3.+bmp(1,1)
	M4p(2) = -bmp(1,2)/3.+bmp(1,1)
	S4m(1) =  bsm(1,2)/3.+bsm(1,1)
	S4m(2) = -bsm(1,2)/3.+bsm(1,1)
	S4p(1) =  bsp(1,2)/3.+bsp(1,1)
	S4p(2) = -bsp(1,2)/3.+bsp(1,1)
	E5m(1) =  bem(2,2)/3.+bem(2,1)
	E5m(2) = -bem(2,2)/3.+bem(2,1)
	E5p(1) =  bep(2,2)/3.+bep(2,1)
	E5p(2) = -bep(2,2)/3.+bep(2,1)
	M5m(1) =  bmm(2,2)/3.+bmm(2,1)
	M5m(2) = -bmm(2,2)/3.+bmm(2,1)
	M5p(1) =  bmp(2,2)/3.+bmp(2,1)
	M5p(2) = -bmp(2,2)/3.+bmp(2,1)
	S5m(1) =  bsm(2,2)/3.+bsm(2,1)
	S5m(2) = -bsm(2,2)/3.+bsm(2,1)
	S5p(1) =  bsp(2,2)/3.+bsp(2,1)
	S5p(2) = -bsp(2,2)/3.+bsp(2,1)

C Construct background multipole amplitudes with I=3/2

	E4m(3) = bem(1,3)
	E5m(3) = bem(2,3)
	E4p(3) = bep(1,3)
	E5p(3) = bep(2,3)
	M4m(3) = bmm(1,3)
	M5m(3) = bmm(2,3)
	M4p(3) = bmp(1,3)
	M5p(3) = bmp(2,3)
	S4m(3) = bsm(1,3)
	S5m(3) = bsm(2,3)
	S4p(3) = bsp(1,3)
	S5p(3) = bsp(2,3)

      RETURN
      END

