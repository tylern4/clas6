c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c     -------
c     
c     Major revisions:
c     By Jixie Zhang @11/17/2008, change nq2 from integer to real
c     ----------------
c     
      real ebeam,wmin,wmax,q2min,q2max,xmin,xmax,tzmin,tzmax,nq2,t_slop,
     &     cohl,ptrg,t_min,t_max
      integer maxevents,maxfiles,npart,kp(10),maxiter,pidfc(10),ffc(10),
     &     stfile,nfc
      real thfcm(10),thfcx(10),phfcm(10),phfcx(10),pfcm(10),pfcx(10),
     &     txy,tdep_slop(10),minmass(10)
      COMMON /parm1/ maxevents,maxfiles,stfile,maxiter,ebeam,wmin,wmax,
     &     q2min,q2max,xmin,xmax,t_slop,nq2,npart,kp,tzmin,tzmax,txy,
     &     cohl,pidfc,ffc,thfcm,thfcx,phfcm,phfcx,pfcm,pfcx,ptrg,
     &     tdep_slop,t_min,t_max,minmass,nfc
c
C     
C     input parameters from psfs_in file
C     
C     maxevents       =       max number of generated (gp) interactions
C     maxiter         =       max number of events per (gp) interaction
C     ebeam           =       Incident electron beam energy
C     wmin,wmax       =       range of total hadron mass, for photon beam
C     range of photon energy
C     q2min,q2max     =       range of photon mass, for real photo =0. =0.
c     xmin,xmax       =       limits of Bjorken X to be generated
C     t_slop          =       produced meson t dependence F(t)=exp(t*t_slop)
C     nq2             =       power of q2 dependence
C     npart           =       number of finale particle
C     kp              =       final particles ID(JETSET) 
C     tzmin           =       target minimum z edge
C     tzmax           =       target maximum z edge
C     txy             =       beam width at the target
C
c      save /parm1/
