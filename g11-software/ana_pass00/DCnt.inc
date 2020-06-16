      integer maxDC
      parameter (maxDC=512)
      integer nDC
      integer secDC
      integer layerDC
      integer WireDC
      integer TDCDC
*
      common /DCnt/ nDC
     &,secDC(maxDC)
     &,layerDC(maxDC)
     &,WireDC(maxDC)
     &,TDCDC(maxDC)
