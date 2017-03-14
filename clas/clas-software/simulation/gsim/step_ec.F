      subroutine step_ec

c_begin_doc

c  Documentation for subroutine step_ec

c  $Id: step_ec.F,v 1.6 2010/01/09 01:48:21 fklein Exp $

c  Purpose:
c  --------
c      ******************************************************************
c      *                                                                *
c      *      Store hits for sampling calorimeter.                      *
c      *      HIT(1):X coordinate at entry in master reference frame    *
c      *      HIT(2):Y                                                  *
c      *      HIT(3):Z                                                  *
c      *      HIT(4):total momentum of the particle                     *
c      *      HIT(7):total energy loss in scintillator strips           *
c      *      HIT(5):time of flight                                     *
c      *                                                                *
c      *      Author: V. Burkert  October, 1988                         *
c      *                                                                *
c      *                                                                *
c      *          MODIFIED by M. Guidal March 93                        *
c      *          ========================================              *
c      *                                                                *
c      *       Modified  February  23, 1994   E.Burtin  		*
c      *                      -> attenuation lenght implemented         *
c      *                      -> TDC optimized                          *
c      *                                                                *
c      *                                                                *
c      ******************************************************************

c  Other routines:
c  ---------------
c     called by user_geom

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 22-nov-94

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: step_ec.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2010/01/09 01:48:21 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'STEP_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: step_ec.F,v 1.6 2010/01/09 01:48:21 fklein Exp $
     1 '/
      save


c  include files:
c  --------------

#include "gcbank.inc"
#include "gcunit.inc"
#include "gcsets.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gctmed.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_ec.inc"
#include "ecgeom.inc"
#include "ecsets.inc"
c_end_inc


c  local variables:
c  ----------------
c
      integer iadr,JK,JK1            ! extract subbank address every time(!) 
      integer iset1,idet1
      CHARACTER*4 volnam(15)
      EQUIVALENCE(names(1),volnam(1))
c temp
c      real edeposit
c      common/edep/edeposit
c temp
      real hith(nhits_ech)
      real hit(nhec),ebxd(3)
      real pareb(35),atteb(11)
      character*4 styp,cfromi
      real acc_len,att
      data acc_len / .4 /


c  tdc_sens = refractive index / (speed of light * cos( average photon angle)
c             n = 1.58   c = 3. 10e8   angle = 15.2 degrees
c             unit : nano-second per centimeter

      real layer_thic,tdc_sens,fiber_ind
      parameter ( layer_thic = .063 )
      parameter ( tdc_sens   = 0.0546 )
      parameter ( fiber_ind  = 1.58 )

      real uran(1),decay_constant,decay_offset

c  the following were automatically generated...ejw
      real att_dis,att_fact,fiber_offset,half_len
      integer ihit,ilayer,natteb,npareb
c_end_var


c  executable code:
c  ----------------

      if(nohits(ec))return

c  store ECH hit if track physically is entering EC mother volume
c   (vs. shower track, created inside EC).

      if(idtype.eq.idtype_ech)then
        if((inwvol.eq.1).and.(nstep.gt.1).and.
     &        (infrom.eq.0).and.(nlevel.eq.4))then
*          call ucopy(vect,hith,7)
*          hith(8)=real(ipart)
*          hith(9)=tofg*1.E+9
*          call gsahit(iset,idet,itra,numbv,hith,ihit)
          iset1=iset
          idet1=idet
          return
        endif
      endif

      if(inwvol.eq.2.and.volnam(nlevel).eq.'ECST'.and.
     &   volnam(nlevel-1).eq.'ECC1'.and.number(nlevel).eq.2) then
        call gmtod(vect,hith,1)
        call gmtod(vect(4),hith(4),2)
        hith(7)=vect(7)
        hith(8)=real(ipart)
        hith(9)=tofg*1.E+9
        call gsahit(iset1,idet1,itra,numbv,hith,ihit)
        return
      endif

c  store EC data hit if track is inside ec sensitive detector volume
      if(idtype.ne.idtype_ec)return

      if(inwvol.eq.1) then

         call ucopy(vect, hit, 3)

c  gets: coordinates in current volume,
c  name of cur. volume and shape parameters
         call gmtod(vect, ebxd, 1)
c         styp=cfromi(names(7))
         styp = volnam(7)
         call gfpara(styp,numbv(4),1,npareb,natteb,pareb,atteb)
	
c  due to geant problem I determine by hand length of the element: half_len
c  distance travelled by light in scintillator using acc_len (increase of
c  length between to layers)
         half_len = (pareb(5)+pareb(9))/2.
         if(styp.eq.'CSUI')then
            ilayer = nviews * numbv(3) - 2
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len - ebxd(1)
         elseif(styp.eq.'CSUO')then
            ilayer = nviews * (numbv(3)+5) - 2
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len - ebxd(1)
         elseif(styp.eq.'CSVI')then
            ilayer = nviews * numbv(3) - 1	   
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len + ebxd(1)
         elseif(styp.eq.'CSVO')then
            ilayer = nviews * (numbv(3)+5) - 1	   
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len + ebxd(1)
         elseif(styp.eq.'CSWI')then
            ilayer = nviews * numbv(3)    
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len + ebxd(1)
         elseif(styp.eq.'CSWO')then
            ilayer = nviews * (numbv(3)+5)
            half_len = half_len + (ilayer - .5 ) * acc_len
            att_dis = half_len + ebxd(1)
         endif


c  attenuation factor is calculated from attenuation length and
c     distance travelled by light in scintillator
         if (atlen.eq.0.) then
            att_fact = 1.
            att_dis = 0.
         else if (atlen.lt.0.) then
            att = att_length(numbv(1),ilayer,numbv(4))
            if (att.gt.0.) then
               att_fact = exp(-att_dis/att)
            else
               att_fact=1.
               call recmes(crname,'I',
     1              '?Bad data for attenuation length')
            endif    
         else   
            att_fact = exp(-att_dis/atlen)
         endif


c  takes into account the delay coming from light transmission
c     the optical fibers
         fiber_offset = (nlayer-ilayer) * layer_thic


c  calculate time offset according to decay constant by drawing a random number 
c     from an exponential
         call grndm(uran,1)
         decay_offset = -log(uran(1)) * decay_constant
         hit(4) = vect(7)
         hit(7) = 0.
         hit(6) = 0.
         hit(5) = tofg*1.e09 + att_dis * tdc_sens + 
     1        fiber_offset + decay_offset

      else ! inwvol.ne.1

c  corrects the deposited energy according to attenuation length
         hit(7) = hit(7) + destep * att_fact
c temp
c         edeposit=edeposit+destep
c temp
c no track bank for 'itra' ?? ... I have to check the ZEBRA structure (fk 1.99)
         JK=LQ(JKINE-itra)
         if(JK.GT.0) then 
            JK1=LQ(JK-1)
            if(JK1.GT.0) then
               iadr=IQ(JK1-1)
            else
               iadr=6
            endif
            if(destep.gt.0.0) then
               if(ilayer.le.15) then
                  call gskinu(itra,1,destep,iadr)
               else
                  call gskinu(itra,1,-destep,iadr)
               endif
            endif
         endif
      endif

      if(inwvol.ne.2.and.istop.eq.0)return
      if (hit(7).le.10.e-9)return

c  store hit
      call gschit(iset,idet,itra,numbv,hit,1,ihit)

      if (ihit .le. 0)then
         write (chmail,1100) itra
         call recmes(crname,'I',chmail)
      end if


 1100 format ('?ECSTEP ERROR  :Hit in calorimeter track'
     1     ,1I4, ' could not be stored.')

      return
      end

c---------------------------------------------------------------------



