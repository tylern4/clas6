      subroutine step_sc

c_begin_doc

c  Documentation for subroutine step_sc

c  $Id: step_sc.F,v 1.3 1998/04/11 03:50:11 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       Store hits for scintillators                             *
c     *       HIT(1): X coordinate at entry in master reference system *
c     *       HIT(2): Y                                                *
c     *       HIT(3): Z                                                *
c     *       HIT(4): Time of flight at entry                          *
c     *       HIT(5): Total energy loss of the track                   *
c     *                                                                *
c     *    ==>CALLED BY : user_step                                    *
c     *       Author    M.Guckes       Oct.  87                        *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     *                                                                *
c     *       modified  E. Burtin      June  94                        *
c     *             - hit structure rearranged                         *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 2-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: step_sc.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/04/11 03:50:11 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'STEP_SC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: step_sc.F,v 1.3 1998/04/11 03:50:11 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "scsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c  ----------------
      real hit(nhits_sc),ebxd(3)
      real vertex(3), p(4), ubuff(1)
      integer nubuff,ihit,itrk,num_vertex,ipart_num
      real hith(nhits_sch)
c_end_var


c  executable code:
c  ----------------

      if(nohits(sc))return

c  SC mc hits: "vect" from common gctrak contains the particle vector, x,y,z,px,py,pz,p

      if(idtype.eq.idtype_sch)then
         if((inwvol.eq.1).and.(nstep.gt.1).and.
     1        (infrom.eq.0))then
            call ucopy(vect,hith,7)         ! Copy entire hit vector.
            hith(8)=real(istak)             ! istak from gckine: track number
            hith(9)=real(ipart)             ! ipart from gckine: particle type.
            call gsahit(iset,idet,itra,numbv,hith,ihit) ! Store hit in JHITS.
            return
         endif
      endif


c  SC data hits  
      if(idtype.eq.idtype_sc)then

C inwvol from common gctrak: 0 is inside volume, 1 entering volume, 2 exiting volume.

         if(inwvol.eq.1) then      ! When entering the volume.
            call ucopy(vect, hit,3)! Copy position from hit vector to "hit".
            hit(4) = 1.e9*tofg     ! tofg from gckine: current time of flight.
            hit(5) = destep        ! destep from gckine: total energy loss in current step.
            return

         else if (inwvol.eq.2. .or. istop.ne.0) then ! exiting volume, or no longer tracking.
	    hit(1) = (hit(1)+vect(1))*.5  ! Average the position of the hit.
            hit(2) = (hit(2)+vect(2))*.5
	    hit(3) = (hit(3)+vect(3))*.5
            hit(4) = (hit(4)+1.e9*tofg)*.5 ! Average the hit time of flight.
            hit(5) = hit(5)+destep         ! Sum the energy loss.

C            call gfkine( itrk, vertex, p, ipart_num, num_vertex,
C     &           ubuff, nubuff )           ! Get the "long life" track parameters.
C            call gmtod(vect, ebxd, 1)      ! Transform vect to SCIN coordinate system put in ebxd.
            
         else                              ! Happily continueing to step through.
            hit(5) = hit(5)+destep
            return
         endif
         
         if (inwvol.ne.2.and.istop.eq.0) return
         if (hit(5).le.10.e-9)           return
         
         call gschit(iset,idet,itra,numbv,hit,1,ihit)! add the results to allother hits in this element.
         
         if (ihit .le. 0) then
            write (chmail,1100) itra
            call recmes(crname,'I',chmail)
         endif
      endif

      return


 1100 FORMAT ('?step_sc ERROR   : Hit in scintillator of track '
     1       ,1I4,' could not be stored.')

      end

c---------------------------------------------------------------------------------

