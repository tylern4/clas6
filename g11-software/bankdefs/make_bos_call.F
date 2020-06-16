      subroutine make_bos_call(iarray_key, bankname, bank_index, ind)
      implicit none
      save
#include "bcs.inc"
#include "wcs.inc"
      character*4 bankname
      integer bank_index
      integer ind
      integer mlink
      integer iarray_key
      if (iarray_key .eq. 0) then
         ind = mlink(iw, bankname, bank_index)
      else
         ind = mlink(jw, bankname, bank_index)
      end if
      return
      end
