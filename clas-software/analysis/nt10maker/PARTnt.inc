      integer maxPART
      
      integer nPRT
      integer pidPART
      real xPART
      real yPART
      real zPART
      real ePART
      real pxPART
      real pyPART
      real pzPART
      real qPART
      integer flagsPART
      
      parameter (maxPART=10)
      
      common /PARTnt/ nPRT
     &,pidPART(maxPART)
     &,xPART(maxPART)
     &,yPART(maxPART)
     &,zPART(maxPART)
     &,ePART(maxPART)
     &,pxPART(maxPART)
     &,pyPART(maxPART)
     &,pzPART(maxPART)
     &,qPART(maxPART)
     &,flagsPART(maxPART)
