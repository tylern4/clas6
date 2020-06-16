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
      integer trkPART
      real qpiPART
      real qtrPART
      integer flgPART
      
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
     &,trkPART(maxPART)
     &,qpiPART(maxPART)
     &,qtrPART(maxPART)
     &,flgPART(maxPART)
