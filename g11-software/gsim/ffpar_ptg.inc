c  ffpar_ptg.inc

c  $Id: ffpar_ptg.inc,v 1.3 1998/12/07 21:07:38 fklein Exp $

      character*132 ciffpar_ptg
      data ciffpar_ptg/
     1'$Id: ffpar_ptg.inc,v 1.3 1998/12/07 21:07:38 fklein Exp $'
     1/


c  ptg angle,field,scale
      integer ptg_ifield
      real ptg_angle,ptg_fieldm,ptg_scale,ptg_maxradius
      common/ffpar_ptg/ptg_angle,ptg_ifield,ptg_fieldm,ptg_scale,ptg_maxradius

