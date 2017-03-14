      Real Function user_costhcm(x)
c
      real x
      real par(4),coscm,coscmf,coscmfr
c from Bryan's data on L(1520)
      data par/0.03538,0.03276,0.02357,0./
c
 101  coscm=-1+2.*getrand(-1.)
      coscmf=par(1) + par(2)*coscm + par(3)*coscm*coscm
      coscmfr=getrand(-1.)*(par(1) + par(2) + par(3))
      if(coscmf.lt.coscmfr) go to 101
      user_costhcm=coscm
c
      return
      end
