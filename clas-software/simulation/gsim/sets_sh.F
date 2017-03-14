      subroutine sets_sh

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_sh.F,v $')
      parameter (crevis= '$Revision: 1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 20:00:00 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'sets_sh')
      parameter (crauth= '')
      data crcsid/'
     1$Id: sets_sh.F,v 1.1 2009/12/22 20:00:00 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gclist.inc"
#include "gcsets.inc"
#include "gconst.inc"
#include "shsets.inc"
c_end_inc

c  local variables:
c  ----------------

c  for data
      character*4 namesv(nvol_hd),namesh(nhits_hd),namesd(ndigi_hd)
      integer nbitsv(nvol_hd),nbitsh(nhits_hd),nbitsd(ndigi_hd)
      real orig(nhits_hd),fact(nhits_hd)
c
        data namesv/'SPIX'/
        data nbitsv/9/
c
      data namesh /'X   ','Y   ','Z   ','TIME','ELOS'/
      data nbitsh / 0,     0,     0,      0,     0 /
      data orig   / 500.,  500.,  500.,  0.,    0./
      data fact   / 100.,  100.,  100., 1.e5,  1.e5/

      data namesd /'ADC ','TDC '/
      data nbitsd /    0,     0/

      integer get_idtype
c_end_var


c  executable code:
c  ----------------

c  set for data
      idtype_hd = get_idtype()
c
      call  gsdet('SH  ','SPIX',nvol_hd,namesv,nbitsv,
     1     idtype_hd,1000,1000,set_hd,det_hd)
      call gsdeth('SH  ','SPIX',nhits_hd,namesh,nbitsh,orig,fact)
      call gsdetd('SH  ','SPIX',ndigi_hd,namesd,nbitsd)
c
      return
      end

c-----------------------------------------------------------------------
