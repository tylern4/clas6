      subroutine step_st

c_begin_doc

c  Documentation for subroutine step_st

c  $Id: step_st.F,v 1.4 2004/09/07 16:50:54 devita Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       Store hits for scintillators                             *
c     *       HIT(1): X coordinate at entry in master reference system *
c     *       HIT(2): Y                                                *
c     *       HIT(3): Z                                                *
c     *       HIT(4): Total momentum of the particle                   *
c     *       HIT(5): Time of flight at entry                          *
c     *       HIT(6): Total energy loss of the track                   *
c     *       HIT(7-9) : X,Y,Z coordinate at the exit                  *
c     *       HIT(10) : Time of flight at exit                         *
c     *                                                                *
c     *    ==>CALLED BY : user_step                                    *
c     *       Author    M.Guckes       Oct.  87                        *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: step_st.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/09/07 16:50:54 $')
      parameter (cautho= '$Author: devita $')
      parameter (crname= 'STEP_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: step_st.F,v 1.4 2004/09/07 16:50:54 devita Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gconst.inc"
#include "stsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_st.inc"
c_end_inc

c  local variables:
c  ----------------
      integer ihit
      real hit(nhits_st),hith(nhits_sth)
c_end_var


c  executable code:
c  ----------------
      	
      if(nohits(st))return
   
c  MC hits
      if(idtype.eq.idtype_st)then
         if((inwvol.eq.1).and.(nstep.gt.1).and.
     1        (infrom.eq.0))then
            call ucopy(vect,hith,7)
            hith(8)=real(istak)
            hith(9)=real(ipart)
            hith(10)=tofg*1.E+9

            if(st_type.eq.0) then
               if (iset.eq.set_st.and.idet.eq.1) then
                  call gsahit(set_st,det_sth1,itra,numbv,hith,ihit)
               else if (iset.eq.set_st.and.idet.eq.2) then
                  call gsahit(set_st,det_sth2,itra,numbv,hith,ihit) 
               endif     
            else
               call gsahit(iset,det_stph11-det_stp11+idet,itra,numbv,hith,ihit) 
            endif
         endif
      endif
      
c  data hits
      if(idtype.eq.idtype_st)then
         if(inwvol.eq.1) then
            call ucopy(vect, hit, 3)
            hit(4) = vect(7)
            hit(6) = destep
            return
            
         elseif (inwvol.eq.2. .or. istop.ne.0) then
            hit(1) = (hit(1)+vect(1))*.5
            hit(2) = (hit(2)+vect(2))*.5
            hit(3) = (hit(3)+vect(3))*.5
            hit(6) = hit(6)+destep
            hit(5) = 1.e9*tofg
                       
         else
            hit(6) = hit(6)+destep
            return
         endif
         
         if (inwvol.ne.2.and.istop.eq.0) return
         if (hit(6).le.100.e-6)          return
         
         call gschit(iset,idet,itra,numbv,hit,1,ihit)    
         
         if (ihit .le. 0) then
            write (chmail,1100) itra
            call recmes(crname,'I',chmail)
         endif

      endif

      return

 1100 FORMAT ('?step_st ERROR : Hit in scintillator of track '
     1       ,1I4,' could not be stored.')

      end

c---------------------------------------------------------------------------------

