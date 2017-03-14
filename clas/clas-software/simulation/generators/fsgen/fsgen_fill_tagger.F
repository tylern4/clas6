      Subroutine fsgen_fill_tagger(e_electron,e_photon,toff,ierr)
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
      integer ierr,i
      data i/0/
      real e_electron,e_photon,toff
c     make TAGR bank
      call make_mctagr(e_electron,e_photon,toff)
c      write(*,*) e_electron,e_photon
c     note that the last parameter is a time offset that can be used 
c     for smearing the time, or adjusting for a vertex not at the origin
c 
      return
      end



