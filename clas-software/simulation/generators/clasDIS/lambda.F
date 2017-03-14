       real function lambda(ip,beam)
       implicit none
#include "ntupgkoko.inc"
       integer ip
c
c      calculating azimuthal angle from lab variables
c      of hadron  
c
c      input variables: pie,pit,pif-momentum,theta,phi of hadron
c                       eleq2,elee,elef-Q^2,momentum,phi of eleitron                 
c
c     CALL CROSS(A,B,C) C=[AxB]
c     VDOT(A,B,N), CALL VMUL(A,B,X,N) X_i=A_i.B_i  N=3  reals
c     VDOTN(A,B,N)=ab/|a||b|
c     VMOD (A,N)  =|a|
       real pi,tetgam,anu,pien,beam
       real eleq,pitg1,cospff1
       real xm,amp,tetgam1,elety,ebeam,vdotm
       real pi4(4),qiu4(4),el04(4),elf4(4),tnorm(4)
       real pro4(4),tnorm2(4)
       real vmass,pt,phi,costh
       pi=acos(-1.0)
        amp=0.938
        ebeam=beam
c
c     define all 4momenta
c
       if(ip.eq.0) then !data
       if(npim.le.0.or.npro.le.0) print *,'wrong set'
c
c pi-
        pien=sqrt(pime*pime-0.139*0.139)
        pi4(4)=pime
        pi4(1)=pien*cos(pimf)*sin(pimt) 
        pi4(2)=pien*sin(pimf)*sin(pimt)
        pi4(3)=pien*cos(pimt)
c
c pro
        pien=sqrt(proe*proe-0.88035)
        pro4(4)=proe
        pro4(1)=pien*cos(prof)*sin(prot) 
        pro4(2)=pien*sin(prof)*sin(prot)
        pro4(3)=pien*cos(prot)
        else                               ! mc
       if(gnpim.le.0.or.gnpro.le.0) print *,'wrong set'
c pi-
        pien=sqrt(gpime*gpime-0.139*0.139)
        pi4(4)=gpime
        pi4(1)=pien*cos(gpimf)*sin(gpimt) 
        pi4(2)=pien*sin(gpimf)*sin(gpimt)
        pi4(3)=pien*cos(gpimt)
c
c pro
        pien=sqrt(gproe*gproe-0.88035)
        pro4(4)=gproe
        pro4(1)=pien*cos(gprof)*sin(gprot) 
        pro4(2)=pien*sin(gprof)*sin(gprot)
        pro4(3)=pien*cos(gprot)

        endif
c
c
         call vsumm(pro4,pi4,tnorm,4)
         lambda=vmass(tnorm)
         return
         end

