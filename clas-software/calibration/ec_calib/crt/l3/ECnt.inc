      integer maxEC
      parameter (maxEC=1296)
      integer nEC
      integer secEC
      integer layEC
      integer strEC
      integer TDCEC
      integer ADCEC
*
      common /ECnt/ nEC
     &,secEC(maxEC)
     &,layEC(maxEC)
     &,strEC(maxEC)
     &,TDCEC(maxEC)
     &,ADCEC(maxEC)
