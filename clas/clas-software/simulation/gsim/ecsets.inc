c  ecsets.inc

c  $Id: ecsets.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $

      character*132 ciecsets
      data ciecsets/
     1'$Id: ecsets.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $'
     1/

      integer nhits_ech
      parameter (nhits_ech=9)

ccc      character*4 cdet_ec(3)
ccc      data cdet_ec/'SCSU','SCSV','SCSW'/

      character*4 cplate_ec(6)
      data cplate_ec/'ECUI','ECVI','ECWI','ECUO','ECVO','ECWO'/

      character*4 cdet_ec(6)
      data cdet_ec/'CSUI','CSVI','CSWI','CSUO','CSVO','CSWO'/

      integer idtype_ec,set_ec,det_ec,idtype_ech,set_ech,det_ech
      common/ecsets/idtype_ec,set_ec,det_ec(6),
     1              idtype_ech,set_ech,det_ech

