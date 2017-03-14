      subroutine clas_nt_fill(ntid,beam_type)
      
      include 'ntup.inc'

      character beam_type
      integer*4 ntid
      integer ierr
      
      ierr=1
      if (lpart_nt_do) call fill_part_nt
      if (lseb_nt_do)  call fill_seb_nt(ierr)
      if (ltgpb_nt_do) call fill_tgpb_nt
      if (ltagr_nt_do) call fill_tagr_nt
      if (ltgbi_nt_do) call fill_tgbi_nt
      if (lmvrt_nt_do) call fill_mvrt_nt
      if (lseb_ntn_do)  call fill_seb_ntn(ierr,beam_type)
      if (lmctk_nt_do)  call fill_mc_nt(ierr)

      if(ierr.gt.0)call hfnt(ntid)
      
      end
