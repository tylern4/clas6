c  ffpar.inc

c  $Id: ffpar.inc,v 1.8 1999/07/30 20:47:56 fklein Exp $

      character*132 ciffpar
      data ciffpar/
     1'$Id: ffpar.inc,v 1.8 1999/07/30 20:47:56 fklein Exp $'
     1/

      integer max_ff
      parameter (max_ff=20)

c  general purpose FFREAD variables
      integer nogeom_ff,nosec_ff,nohits_ff,nodigi_ff,
     1     nobosout_ff,nomate_ff,nodata_ff,nomcdata_ff,fast_code_ff
      integer iflgk_ff,nstepmax_ff,skip_ff
      real slengmax_ff,zmin_ff,zmax_ff,rmax_ff,noacc_ff
      real Beam_energy_ff, Beam_offset_ff, Beam_sigma_ff

      common/ffpar/nogeom_ff(max_ff),nosec_ff(max_ff),
     1     nohits_ff(max_ff),nodigi_ff(max_ff),nobosout_ff(max_ff),
     1     iflgk_ff,nstepmax_ff,slengmax_ff,zmin_ff,zmax_ff,rmax_ff,
     1     nomate_ff(max_ff),
     1     nodata_ff(max_ff),nomcdata_ff(max_ff),fast_code_ff(max_ff),
     1     skip_ff,noacc_ff(max_ff),
     &     Beam_energy_ff(2),Beam_offset_ff(2),Beam_sigma_ff


