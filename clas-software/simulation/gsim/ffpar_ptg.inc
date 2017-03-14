c  ffpar_ptg.inc

c  $Id: ffpar_ptg.inc,v 1.4 2011/03/31 19:52:57 fxgirod Exp $

      character*132 ciffpar_ptg
      data ciffpar_ptg/
     1'$Id: ffpar_ptg.inc,v 1.4 2011/03/31 19:52:57 fxgirod Exp $'
     1/


c  ptg angle,field,scale
      integer ptg_ifield
      real ptg_angle(3),ptg_fieldm,ptg_scale,ptg_maxradius
      common/ffpar_ptg/ptg_angle,ptg_ifield,ptg_fieldm,ptg_scale,ptg_maxradius

