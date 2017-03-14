      integer maxEC1
      parameter (maxEC1=256)
      integer nEC1
      integer sector
      integer layer
      integer strip
      integer TDCL
      integer ADCL
      integer TDCR
      integer ADCR
      real *4 T_l,T_r,E_l,E_r
*
      common /EC1nt/ nEC1
     &,sector(maxEC1)
     &,layer(maxEC1)
     &,strip(maxEC1)
     &,TDCL (maxEC1)
     &,ADCL (maxEC1)
     &,TDCR (maxEC1)
     &,ADCR (maxEC1)
     &,T_l (maxEC1)
     &,E_l (maxEC1)
     &,T_r (maxEC1)
     &,E_r (maxEC1)
     
