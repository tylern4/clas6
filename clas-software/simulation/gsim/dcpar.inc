c  dcpar.inc

c  $Id: dcpar.inc,v 1.3 1998/04/15 22:18:51 kjoo Exp $

      character*132 cidcpar
      data cidcpar/
     1'$Id: dcpar.inc,v 1.3 1998/04/15 22:18:51 kjoo Exp $'
     1/

c  revisions:
c  Kyungseon Joo, 14-Apr-1997
c   - change nddc to 4 to add doca 
c
c  from patch  in file glas.car on Tue Nov 15 17:33:58 1994
c  this is seq: DCPAR
c-----------------------------------------------------------------------
c   DCPAR - Contains parameters and variables for the drift chambers
c   PARAMETERS: NVDC        : Number of volume descriptors
c               NHDC        : Number of elements per hit
c               NDDC        : Number of elements per digitisation
c-----------------------------------------------------------------------

c  number of dc super layers
      integer ndc_sl
      parameter (ndc_sl=6)

      integer nvdc,nhdc,nddc
      parameter (nvdc=3, nhdc=12, nddc=4)
