      subroutine ntup_init(data_type, beam_type)
      
      implicit none
      save

#include "ntup.inc"

      integer data_type
      character beam_type
      
      if(data_type.eq.8) lmctk_nt_do	= .true.
      lpart_nt_do	= .false.
      if(data_type.eq.2) lpart_nt_do = .true.
      lhead_nt_do	= .false.
      lbeam_nt_do	= .false.
      
      lec_nt_do		= .false.
      lsc_nt_do		= .false.
      lcc_nt_do		= .false.
      lec1_nt_do	= .false.
      
      lechb_nt_do	= .false.
      lccrc_nt_do	= .false.
      ltrk_nt_do	= .false.
      if(data_type.eq.11.or.data_type.eq.13)then
         lseb_nt_do = .false.
         lseb_ntn_do= .true.
      elseif(data_type.eq.1.or.data_type.eq.3)then
         lseb_ntn_do= .false.
         lseb_nt_do	= .true.
         if(data_type.eq.3)lmctk_nt_do=.true.
      endif
      lhbtr_nt_do	= .false.
      if (beam_type .eq. 'g') then
         ltgpb_nt_do = .true.
         ltagr_nt_do = .true.
      else
         ltgpb_nt_do = .false.
         ltagr_nt_do = .false.
      end if

      end

