c  scsets.inc

c  $Id: scsets.inc,v 1.3 1998/04/26 09:51:06 holtrop Exp $

      character*132 ciscsets
      data ciscsets/
     1'$Id: scsets.inc,v 1.3 1998/04/26 09:51:06 holtrop Exp $'
     1/

      integer nvol_sc,nhits_sc,ndigi_sc
      parameter (nvol_sc=2)
      parameter (nhits_sc=5)
      parameter (ndigi_sc=4)


      integer nhits_sch
      parameter (nhits_sch=9)

      integer idtype_sc,set_sc,det_sc
      integer idtype_sch,set_sch,det_sch
      common/scsets/idtype_sc,set_sc,det_sc,
     1     idtype_sch,set_sch,det_sch

