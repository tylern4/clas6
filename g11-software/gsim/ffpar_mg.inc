c  ffpar_mg.inc

c  $Id: ffpar_mg.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $

      character*132 ciffpar_mg
      data ciffpar_mg/
     1'$Id: ffpar_mg.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $'
     1/


c  ffkey def's for magnet...note that order is important for FFREAD

	integer magtype,init_ifield
        real torus_field,magscale,init_fieldm

	common/ffpar_mg/magtype,torus_field,magscale(2),
     1       init_ifield,init_fieldm

