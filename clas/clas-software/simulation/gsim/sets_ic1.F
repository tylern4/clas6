      subroutine sets_ic1

      implicit none
      save

#include "gcflag.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gcsets.inc"
#include "gconst.inc"
#include "icgeom.inc"
#include "icsets.inc"

      character*4 namesv(nvic1), namesh(nhic1), namesd(ndic1)
      integer nbitsv,nbitsd,nbitsh
      real fact,orig
      dimension   nbitsv(nvic1),nbitsh(nhic1),orig(nhic1),fact(nhic1)
     1           ,nbitsd(ndic1)


      data namesv/'IC1 '/
      data nbitsv /9/
      
      data namesh /'X   ','Y   ','Z   ','TIME','ELOS'/
      data nbitsh /    0,     0,     0,     0,     0/
      data orig   /  500.,  500.,  500.,    0.,    0./
      data fact   /  100.,  100.,  100.,  1.e4,  1.e4/
      
      data namesd /'ADC ','TDC '/
      data nbitsd /0,0/

      integer get_idtype

c      print *,'hello sets_ic1'

      idtype_ic1=get_idtype()
      call gsdet('IC  ','IC1 ',nvic1,namesv,nbitsv,idtype_ic1,
     11000,1000,set_ic1,det_ic1)
      call gsdeth('IC  ','IC1 ',nhic1,namesh,nbitsh,orig,fact)
      call gsdetd('IC  ','IC1 ',ndic1,namesd,nbitsd)
      
      return
      end


