c     SCnt.inc 
      integer maxSC
      parameter (maxSC=288)
      integer nSC
      integer secSC
      integer idSC
      integer TDCLSC
      integer ADCLSC
      integer TDCRSC
      integer ADCRSC
*
      common /SCnt/ nSC
     &,secSC(maxSC)
     &,idSC(maxSC)
     &,TDCLSC(maxSC)
     &,ADCLSC(maxSC)
     &,TDCRSC(maxSC)
     &,ADCRSC(maxSC)
