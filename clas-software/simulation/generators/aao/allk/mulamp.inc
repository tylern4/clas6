      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      dimension phase(nedim,5,2),phaset(nedim,5,2)
      dimension
     &  cdmul (nedim,4,0:ladim,8),cbmul (nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)
     
      integer mulout(6)
      character*3 cmulout(6)
      data mulout/7,8,1,2,3,4/
      data cmulout/'SL+','SL-','EL+','EL-','ML+','ML-'/
