      integer function calreadec1map(unit)
      
      integer unit,sec,lay,str,roc,slot
      integer type,chan,dum2
      character*2 dum,bank
      logical devadc,devtdc
      
      vector ec1adcmap(40,4,6)
      vector ec1tdcmap(40,4,6)
      vector rocmap(3,64,25)
      vector croc
      
      read(unit,*) dum
      read(unit,*) dum
      read(unit,*) dum
      
      do i = 1,1024
      read(unit,*,end=20) bank,sec,lay,str,dum2,dum2,dum2,roc
     *,slot,type,chan
      devadc = type.eq.1881
      devtdc = type.eq.1872
      if (devadc) then
        ec1adcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 1 
      endif
      if (devtdc) then
        ec1tdcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 0
      endif
      enddo
      
20    calreadec1map = 1
      
      end
      
      integer function calreadecmap(unit)
      
      integer unit,sec,lay,str,roc,slot
      integer type,chan,dum2
      character*2 dum,bank
      logical devadc,devtdc
      
      vector ecadcmap(36,6,6)
      vector ectdcmap(36,6,6)
      vector rocmap(3,64,25)
      vector croc
      
      read(unit,*) dum
      read(unit,*) dum
      read(unit,*) dum
      
      do i = 1,1296
      read(unit,*,end=20) bank,sec,lay,str,dum2,dum2,dum2,roc
     *,slot,type,chan
      devadc = type.eq.1881
      devtdc = type.eq.1872
      if (devadc) then
        ecadcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 1 
      endif
      if (devtdc) then
        ectdcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 0
      endif
      enddo
      
20    calreadecmap = 1
      
      end
      
      integer function calreadscmap(unit)
      
      integer unit,sec,str,lay,roc,slot
      integer type,chan,dum2
      character*2 dum,bank
      logical devadc,devtdc,pmtl,pmtr
      
      vector scadcmap(48,2,6)
      vector sctdcmap(48,2,6)
      vector rocmap(3,64,25)
      vector croc
      
      read(unit,*) dum
      read(unit,*) dum
      read(unit,*) dum
      
      do i = 1,1152
      read(unit,*,end=20) bank,sec,dum2,str,lay,dum2,dum2,roc
     *,slot,type,chan
      devadc = type.eq.1881
      devtdc = type.eq.1872
      pmtl   = lay.eq.0.or.lay.eq.1
      pmtr   = lay.eq.2.or.lay.eq.3
      if (pmtl) lay = 1
      if (pmtr) lay = 2
      if (devadc) then
        scadcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 1
      endif
      if (devtdc) then
        sctdcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 0
      endif
      enddo
      
20    calreadscmap = 1
      
      end
      integer function calreadccmap(unit)
      
      integer unit,sec,sct,str,lay,j,roc,slot
      integer type,chan,dum2
      character*2 dum,bank
      logical devadc,devtdc
      
      vector ccadcmap(18,2,6)
      vector cctdcmap(18,2,6)
      vector tagttdcmap(61,2,2)
      vector rocmap(3,64,25)
      vector croc
      
      read(unit,*) dum
      read(unit,*) dum
      read(unit,*) dum
      
      do i = 1,432
      read(unit,*,end=20) bank,sec,dum2,sct,dum2,dum2,dum2,roc
     *,slot,type,chan     
      devadc = type.eq.1881
      devtdc = type.eq.1872
      j = mod(sct,2)
      str = sct/2+j
      lay = 2-j
      if (devadc) then
        ccadcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 1
      endif
      if (devtdc) then
        cctdcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 0
      endif
      enddo
      
      do i = 1,122
      read(unit,*,end=20) bank,sec,lay,str,dum2,dum2,dum2,roc
     *,slot,type,chan     
      devtdc = type.eq.1872
      if (devtdc) then
        tagttdcmap(str,lay,sec) = roc*1e4+slot*1e2+chan
        rocmap(1,chan+1,slot) = str
        rocmap(2,chan+1,slot) = lay
        rocmap(3,chan+1,slot) = sec
        croc(slot) = 0
      endif
      enddo
      
20    calreadccmap = 1
      
      end
