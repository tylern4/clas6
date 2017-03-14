       complex*16 function zformf(g,cut,L,p,istar,icnl,ic)
       implicit real*8(a-h,o-y),complex*16 (z)
       parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
       parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
       character*3 allwaves(maxlsj)
       data pi/3.14159d00/,amn/938.5d0/,api/138.5d0/
       complex*16 p,en,epi
       common/cutpar/cut0(maxres,maxlsj)
       common/waves/allwaves
       common/quark/iquark
       common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
       common/chdat2/nch,ich0(maxmb)
c       common / cvertqm /cefc(10,0:5,0:1),cefd(10,0:5,0:1),mxcf

c Phenomenological Form
c sato phases 06:

c pi N
       if (ich0(ic).eq.1) zphd=cmplx(0.,-1.)
c eta N
       if (ich0(ic).eq.2) zphd=cmplx(0.,-1.)
c Delta pi
       if (ich0(ic).eq.3) zphd=cmplx(0.,-1.)
c sigma N
       if (ich0(ic).eq.4) zphd=1.
c rho N
       if (ich0(ic).eq.5) zphd=1.

       skr0=cut0(istar,icnl)
       zformf=zphd * g/sqrt((2.*pi)**3)/sqrt(amn)
     1 *(cut**2/(cut**2+(p-skr0)**2))**(2+L/2.)*(p/api)**(L/2.)

c       write(*,*)'****',zphd,g,zformf,'****'
c
c      Delta -> pi N of SL
c
       if (allwaves(icnl).eq.'P33') then 
       fpinn=sqrt(4.*pi*0.08)
       gpind=sqrt(72.d0/25.d0)*g*fpinn
       epi=sqrt(p**2+api**2)
       en=sqrt(p**2+amn**2)

       zformf=zphd* gpind/api/sqrt((2.*pi)**3)/sqrt(2*epi)
     1 *sqrt((en+amn)/(2*en))*p
     2 *(cut**2/(cut**2+(p-skr0)**2))**2*sqrt(4.*pi/3.)
       endif


       return
       end
