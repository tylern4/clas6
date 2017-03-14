      subroutine clas_geom

c_begin_doc

c  Documentation for subroutine clas_geom

c  $Id: clas_geom.F,v 1.12 2011/04/26 15:52:37 hyonsuk Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Steering routine for CLAS geometry                        *
c     *                                                                *
c     *      Defines the master reference system 'CLAS' (a TUBE)       *
c     *      and divides it to NSECT sectors: 'LSS-'.                  *
c     *                                                                *
c     *       Author    M.Guidal                                       *
c     *                                                                *
c     *       Taken from M.Guckes, complete reorganization of the      *
c     *       Mother structure, based on a cylindrical symmetry        *  
c     *                                                                *
c     ******************************************************************

c  Author:
c  -------
c     original by M.Guidal (taken from M.Guckes), then modified by E.Burtin, CEBAF

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 18-nov-94
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_geom.F,v $')
      parameter (crevis= '$Revision: 1.12 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/04/26 15:52:37 $')
      parameter (cautho= '$Author: hyonsuk $')
      parameter (crname= 'CLAS_GEOM')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_geom.F,v 1.12 2011/04/26 15:52:37 hyonsuk Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar.inc"
#include "lcgeom.inc"
#include "cltmed.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_st.inc" 
#include "ffpar_ic.inc"
c_end_inc

c  local variables:
c   ----------------

c *** must come from BOS banks ***
      real psphe(3) /0.,589.,625./
      real para2(3) /0.,588.,625./
      integer ivol,tmed
c_end_var


c  executable code:
c  ----------------

c  define mother volume CLAS, LAS2 rotated w/r/to CLAS,
c   and divide into 6 sectors LSS-
      if(helium_only)then
         tmed=clas_med_helium
      else
         tmed=clas_med_air
      endif
      call gsvolu('CLAS','TUBE',tmed,psphe,3,ivol)
      call gsvolu('LAS2','TUBE',tmed, para2, 3, ivol)
      call gspos('LAS2',1,'CLAS', 0.,0.,0.,0,'MANY')   !overlaps with PTGM
      call gsdvn2('LSS-','LAS2',nsect,2,330.,tmed)

c  define detector geometry
      if(.not.nogeom(toru).or..not.nogeom(mini))call geom_mg
      if(.not.nogeom(cc))   call geom_cc
      if(.not.nogeom(dc))   call geom_dc_new
      if(.not.nogeom(ec))   call geom_ec
      if(.not.nogeom(ec1))  call geom_ec1      
      if(.not.nogeom(sc))   call geom_sc
      if(.not.nogeom(sh))   call geom_sh
c
      if(.not.nogeom(st)) then
         if(st_type.eq.0) then
            call geom_st
         else
            call geom_stnew
         endif
         nogeom(foil)=.TRUE.
      endif
c
       if(tg_e2)then
          nogeom(tg)=.TRUE.
          nogeom(ptg)=.TRUE.
          call ugeom_tg_e2
       endif
c
      if(.not.nogeom(foil)) call geom_foil
c
      if(.not.nogeom(ic)) then
          if (abs(ic_mod).ge.2) call geom_ic1
      endif
      if(.not.nogeom(sol))  call geom_sol
      if(.not.nogeom(tg))   call geom_tg

c
      if(.not.nogeom(ptg))  call geom_ptg     

c
ccc      if(.not.nogeom(???)call geom_support


c  define detector sets and digi banks
      if(.not.nohits(all))then
         if(nogeom(cc)) nohits(cc)=.TRUE.
         if(.not.nohits(cc))  call sets_cc
         if(nogeom(dc)) nohits(dc)=.TRUE.
         if(.not.nohits(dc))  call sets_dc
         if(nogeom(ec)) nohits(ec)=.TRUE.
         if(.not.nohits(ec))  call sets_ec
         if(nogeom(ec1)) nohits(ec1)=.TRUE.
         if(.not.nohits(ec1)) call sets_ec1
         if(nogeom(sc)) nohits(sc)=.TRUE.
         if(.not.nohits(sc))  call sets_sc
         if(nogeom(sh)) nohits(sh)=.TRUE.
         if(.not.nohits(sh))  call sets_sh
         if(nogeom(st)) nohits(st)=.TRUE.
       
c
         if(.not.nohits(ic)) then
           if (abs(ic_mod).ge.2) call sets_ic1
         endif         
c
         if(.not.nohits(st)) then
            if(st_type.eq.0) then
               call sets_st
            else
               call sets_stnew
            endif
         endif  
      endif

      return
      end

c------------------------------------------------------------------------

