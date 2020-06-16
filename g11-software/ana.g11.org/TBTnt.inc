      integer maxTBTR
      parameter (maxTBTR=100)
      integer nTBTR,secTBTR,hbtTBTR
	real xTBTR,yTBTR,zTBTR,pxTBTR,pyTBTR,pzTBTR,qTBTR,c2TBTR
      common /TBTRnt/ nTBTR
     & ,xTBTR(maxTBTR)
     & ,yTBTR(maxTBTR)
     & ,zTBTR(maxTBTR)
     & ,pxTBTR(maxTBTR)
     & ,pyTBTR(maxTBTR)
     & ,pzTBTR(maxTBTR)
     & ,qTBTR(maxTBTR)
     & ,c2TBTR(maxTBTR)
     & ,secTBTR(maxTBTR)
     & ,hbtTBTR(maxTBTR)

      integer maxTBER
      parameter (maxTBER=100)
	integer nTBER,lay1TBER,lay2TBER
	real qpTBER,lbTBER,phiTBER,d0TBER,z0TBER
     & ,c11TBER,c12TBER,c13TBER,c14TBER,c15TBER
     & ,c22TBER,c23TBER,c24TBER,c25TBER
     & ,c33TBER,c34TBER,c35TBER
     & ,c44TBER,c45TBER
     & ,c55TBER
     & ,chi2TBER
      common /TBERnt/ nTBER
     & ,qpTBER (maxTBER),lbTBER (maxTBER),phiTBER(maxTBER),d0TBER (maxTBER),z0TBER (maxTBER)
     & ,c11TBER(maxTBER),c12TBER(maxTBER),c13TBER(maxTBER),c14TBER(maxTBER),c15TBER(maxTBER)
     & ,c22TBER(maxTBER),c23TBER(maxTBER),c24TBER(maxTBER),c25TBER(maxTBER)
     & ,c33TBER(maxTBER),c34TBER(maxTBER),c35TBER(maxTBER)
     & ,c44TBER(maxTBER),c45TBER(maxTBER)
     & ,c55TBER(maxTBER)
     & ,chi2TBER(maxTBER)
     & ,lay1TBER(maxTBER)
     & ,lay2TBER(maxTBER)
