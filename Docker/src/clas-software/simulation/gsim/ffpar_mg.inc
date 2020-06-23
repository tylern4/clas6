c  ffpar_mg.inc

c  $Id: ffpar_mg.inc,v 1.4 2009/12/22 14:59:09 natasha Exp $

      character*132 ciffpar_mg
      data ciffpar_mg/
     1'$Id: ffpar_mg.inc,v 1.4 2009/12/22 14:59:09 natasha Exp $'
     1/


c  ffkey def's for magnet...note that order is important for FFREAD

	integer magtype,init_ifield
        real torus_field,magscale,init_fieldm,mg_pos_ff,mgzmax,
     1       soln_pos

	common/ffpar_mg/magtype,torus_field,magscale(2),
     1       init_ifield,init_fieldm,mg_pos_ff(3),mgzmax,
     1       soln_pos

