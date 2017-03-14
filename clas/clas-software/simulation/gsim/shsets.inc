c  shsets.inc

c
      integer nvol_hd, nvol_hd1,nhits_hd,ndigi_hd
      parameter (nvol_hd=1)
      parameter (nhits_hd=5)
      parameter (ndigi_hd=2)
c
      integer nhits_hdh
      parameter (nhits_hdh=9)

      integer idtype_hd,set_hd,det_hd
      integer idtype_hdh,set_hdh,det_hdh
      common/hdsets/idtype_hd,set_hd,det_hd,
     1     idtype_hdh,set_hdh,det_hdh

