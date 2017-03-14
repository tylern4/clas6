c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
      REAL PMASS
      PARAMETER (PMASS = 0.938)
      REAL Qv,THETA_Q,PHI_Q
      REAL etarget,ptarget,ptarg(3)
      REAL espect,pspect,spmass
      REAL W,E1,PHMASS,Tmass, wt0, Tmass_all
      REAL TOTMASS, RMASS, T, qx,qy,qz
      Integer IDtarget,IDtargetN, IDspect
      common/parm3/W,E1,PHMASS,etarget,ptarget,ptarg,wt0,espect,pspect,
     +spmass,
     +TOTMASS,RMASS,T,Tmass,Tmass_all,IDtarget,IDtargetN,IDspect,
     +qv,qx,qy,qz,THETA_Q,PHI_Q
