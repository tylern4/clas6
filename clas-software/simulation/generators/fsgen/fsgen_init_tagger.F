      Subroutine fsgen_init_tagger(e_electron,ierr)
c  ---------------
c
c  Notes:
c  ------
c
c  Author:        Created:  Sun Jan 24 22:32:25 EST 1997
c  -------
c
c  Major revisions:
c     
c
      implicit none
#include "bcs.inc"
#include "wcs.inc"
c
      integer ierr
      real e_electron

c     call routines needed by make_mctagr, info about tagger bins
      call fill_ebin_map
      call fill_tbin_map 
      call fill_e_t_map
c
c 
      return
      end
