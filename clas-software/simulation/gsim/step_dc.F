      subroutine step_dc

c_begin_doc

c  Documentation for subroutine step_dc

c  $Id: step_dc.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Store hits for drift chamber.                             *
c     *      As hit is considered then point in the middle between     *
c     *      entry and exit of track.                                  *
c     *       HIT(1): X coordinate in master reference system          *
c     *       HIT(2): Y                                                *
c     *       HIT(3): Z                                                *
c     *       HIT(4): Size of the current tracking step                *
c     *       HIT(5): Energy loss of the track                         *
c     *       HIT(6): Total momentum of the particle                   *
c     *       HIT(7): Time of flight [micro sec]                       *
c     *       HIT(8,9,10) : Cosines director			       *
c     *                                                                *
c     *    ==>CALLED BY : user_step                                       *
c     *       Author    M.Guckes       July  88                        *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 2-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: step_dc.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:54 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'STEP_DC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: step_dc.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcunit.inc"
#include "gccuts.inc"
#include "gcsets.inc"
#include "gcvolu.inc"
#include "gcphys.inc"
#include "gctmed.inc"
#include "gctrak.inc"
#include "gckine.inc"

#include "dcpar.inc"
#include "dcsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c  ----------------
      integer ihit
      real hit(nhdc)
c_end_var


c  executable code:
c  ----------------

      if(nohits(dc))return

      if(idtype.eq.idtype_dc)then
         if(abs(charge).gt.0.)then
            if (inwvol.eq.1) then
               call ucopy(vect, hit, 3)
               hit(4) = step
               hit(5) = destep  !shouldn't this be zero to be safe ???
               hit(6) = vect(7)
               hit(8) = vect(4)
               hit(9) = vect(5)
               hit(10) = vect(6)
               return
               
            else if (inwvol.eq.2. .or. istop.ne.0) then
               hit(1)  = (hit(1)+vect(1))*.5
               hit(2)  = (hit(2)+vect(2))*.5
               hit(3)  = (hit(3)+vect(3))*.5
               hit(4)  = hit(4)+step
               hit(5)  = hit(5)+destep
               hit(7)  = 1.e9*tofg
               hit(8)  = (hit(8)+vect(4))*.5
               hit(9)  = (hit(9)+vect(5))*.5
               hit(10) = (hit(10)+vect(6))*.5
               hit(11) = real(istak)
               hit(12) = real(ipart)

            else
               hit(4) = hit(4)+step
               hit(5) = hit(5)+destep
               return
            end if


c  Neutral particles
c  ??? what does destep for neutral particle mean ???
         else
            if (destep .lt. 1.0e-9) return
            call ucopy(vect, hit, 3)
            hit(4) = step
            hit(5) = destep
            hit(6) = vect(7)
            hit(7) = 1.e9*tofg
            hit(8) = vect(4)
            hit(9) = vect(5)
            hit(10) = vect(6)
            hit(11) = real(istak)
            hit(12) = real(ipart)
         endif
      
c  Store hits
         call gsahit(iset,idet,itra,numbv,hit,ihit)

         if (ihit .le. 0) then
            write (chmail,1100) itra
            call recmes(crname,'I',chmail)
         endif

      endif
         
      return

 1100 format ('?DC hit from track ',i6,' could not be stored.')

      end

c---------------------------------------------------------------------------------
