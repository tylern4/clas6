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
      
      parameter (maxPART=20)
      
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


      integer maxTBID
      parameter (maxTBID=100)
	integer nTBID,trkTBID,secTBID,scsTBID,sciTBID,ecsTBID,eciTBID
     & ,stsTBID,stiTBID,lcsTBID,lciTBID 
	real bTBID,vtTBID
     & ,sctTBID,scqTBID,scvTBID,scbTBID
     & ,ectTBID,ecqTBID,ecvTBID,ecbTBID
     & ,sttTBID,stqTBID,stvTBID,stbTBID
     & ,lctTBID,lcqTBID,lcvTBID,lcbTBID 
      common /TBIDnt/ nTBID
     & ,trkTBID(maxTBID),secTBID(maxTBID),bTBID(maxTBID),vtTBID(maxTBID)
     & ,scsTBID(maxTBID),sciTBID(maxTBID),sctTBID(maxTBID),scqTBID(maxTBID),scvTBID(maxTBID),scbTBID(maxTBID)
     & ,ecsTBID(maxTBID),eciTBID(maxTBID),ectTBID(maxTBID),ecqTBID(maxTBID),ecvTBID(maxTBID),ecbTBID(maxTBID)
     & ,stsTBID(maxTBID),stiTBID(maxTBID),sttTBID(maxTBID),stqTBID(maxTBID),stvTBID(maxTBID),stbTBID(maxTBID)
     & ,lcsTBID(maxTBID),lciTBID(maxTBID),lctTBID(maxTBID),lcqTBID(maxTBID),lcvTBID(maxTBID),lcbTBID(maxTBID) 
