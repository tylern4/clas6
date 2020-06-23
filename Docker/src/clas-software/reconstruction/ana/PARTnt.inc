      integer maxPART
      
      integer nPRT
      integer pidPART,IPART10,IPART13
      real xPART
      real yPART
      real zPART
      real ePART
      real pxPART
      real pyPART
      real pzPART
      real qPART
      real RPART11
      real RPART12
      
      parameter (maxPART=40)
      
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
     &,IPART10(maxPART)
     &,RPART11(maxPART)
     &,RPART12(maxPART)
     &,IPART13(maxPART)
