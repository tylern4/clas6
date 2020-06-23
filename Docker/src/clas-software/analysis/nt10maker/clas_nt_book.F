      subroutine clas_nt_book(ntid, beam_type)
      
      implicit none
      save
            
#include "ntup.inc"
      character beam_type
      integer ntid
c
      call hbnt(ntid,'CLAS',' ')
      call hbname(ntid,' ',0,'$clear')
      if (lpart_nt_do) call book_part_nt(ntid)
      if (lseb_nt_do)  call book_seb_nt(ntid, beam_type)
      if (lseb_ntn_do)  call book_seb_ntn(ntid, beam_type)
      if (lmctk_nt_do)  call book_mc_nt(ntid)
      if (ltgpb_nt_do) call book_tgpb_nt(ntid)
      if (ltagr_nt_do) call book_tagr_nt(ntid)
      end


