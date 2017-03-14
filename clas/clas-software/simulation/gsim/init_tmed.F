      subroutine init_tmed

c_begin_doc

c  Documentation for subroutine init_tmed

c  $Id: init_tmed.F,v 1.13 2009/12/22 15:02:47 natasha Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      CLAS tracking medium definition                           *
c     *                                                                *
c     *       Author    M.Guckes  *******      (Jan 1986)              *
c     *       modified: M.Guckes    Oct.  1987                         *
c     *                 V.Burkert   May   1988                         *
c     *                 M. Guidal   March 93                           *
c     *                 C. Tur      Aug 04                             *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 18-nov-94
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_tmed.F,v $')
      parameter (crevis= '$Revision: 1.13 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 15:02:47 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'INIT_TMED')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_tmed.F,v 1.13 2009/12/22 15:02:47 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gccuts.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_mg.inc"
#include "bgrid.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
c_end_inc

c  local variables:
c  ----------------
      integer get_tmed,get_mate, mate
c_end_var


c  executable code:
c  ----------------

c  clas helium 
      clas_med_helium = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_helium
      endif
      call gstmed(clas_med_helium,'CLAS He$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c
      clas_med_helium6 = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_helium6
      endif
      call gstmed(clas_med_helium6,'CLAS He 6$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c
c  clas liq_helium3 
      clas_med_liq_he3 = get_tmed() 
      if(nomate(other))then
         mate=16
      else
         mate=imate_liq_he3
      endif
      call gstmed(clas_med_liq_he3,'CLAS L_He3$', mate,
     1     0,2,100.,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c     1     0,init_ifield,init_fieldm,
c
c  clas helium4 
      clas_med_helium4 = get_tmed() 
      if(nomate(other))then
         mate=16
      else
         mate=imate_liq_he4
      endif
      call gstmed(clas_med_helium4,'CLAS He4$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  clas Al_foil
      clas_med_Al_foil = get_tmed() 
      if(nomate(other))then
         mate=16  
      else
ccc??? difference ???         mate=clas_mat_Al_foil
       clas_mat_Al_foil = get_mate()
       call gsmate(clas_mat_Al_foil, 'CLAS Al_FOIL$',
     +               26.980, 13.0, 0.44, 8.9, 613.0, 0,0)
         mate=clas_mat_Al_foil
      endif
      call gstmed(clas_med_Al_foil,'CLAS AL_FOL$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  clas air
      clas_med_air = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_air
      endif
      call gstmed(clas_med_air,'CLAS Air$',mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)

c  clas mylar...why the GHCOR corrections?
      clas_med_mylar = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_mylar
      endif
      call gstmed(clas_med_mylar,'CLAS Mylar$',mate,
     1     0,init_ifield,init_fieldm,
     1     0.3,   0.5,   .05,   0.005,  0.003, 0, 0)
      call gstpar(clas_med_mylar,'GHCOR1',1.0)
      call gstpar(clas_med_mylar,'GHCOR2',0.7)
      call gstpar(clas_med_mylar,'GHCOR3',1.2)

      call gstpar(clas_med_mylar,'GHCOR4',0.5)

c  clas kapton
      clas_med_kapton = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_kapton
      endif
      call gstmed(clas_med_kapton,'CLAS Kapton$',mate,
     1     0,init_ifield,init_fieldm,
     1     0.3,   0.5,   .05,   0.005,  0.003, 0, 0)


c  these belong in detector routines

      clas_med_vac = get_tmed()
      mate=16
      call gstmed( clas_med_vac,'CLAS Vacuum$',mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   5.0,   0.05, 0.005, 0.003, 0, 0)
c
      clas_med_carbon = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_carbon
      endif
      call gstmed(clas_med_carbon,'Carbon$',        mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)

c
      clas_med_lead = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_lead
      endif
      call gstmed(clas_med_lead,'Lead - Foil$',      mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,  0.05 ,0.005, 0.003, 0, 0)

c
      clas_med_beryl = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_beryllium
      endif
      call gstmed(clas_med_beryl,'Beryllium Tube$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)

c
      clas_med_copper = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_copper
      endif
      call gstmed( clas_med_copper,'Coil - Copper$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
	
c
      clas_med_iron = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_iron
      endif
      call gstmed(clas_med_iron,'Support - Iron$',  mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)

c
      clas_med_alu = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_aluminum
      endif
      call gstmed(clas_med_alu ,'Aluminium$',       mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,    0.05, 0.005, 0.003, 0, 0)

c Stainless steel added in by S. Niccolai to be used in geom_ptg

      clas_med_stainless_steel = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_stainless_steel
      endif
      call gstmed(clas_med_stainless_steel ,'Stainless Steel$',    mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,    0.05, 0.005, 0.003, 0, 0)
          

c
      clas_med_titanium = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_titanium
      endif
      call gstmed(clas_med_titanium ,'Titanium - Foil$',       mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,    0.05, 0.005, 0.003, 0, 0)

c
      clas_med_rigidgraphite = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_rigidgraphite
      endif
      call gstmed(clas_med_rigidgraphite ,'Rigid Graphite$',       mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,    0.05, 0.005, 0.003, 0, 0)

c
      clas_med_flexgraphite = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_flexgraphite
      endif
      call gstmed(clas_med_flexgraphite ,'Flexible Graphite$',       mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,    0.05, 0.005, 0.003, 0, 0)

c
      clas_med_h2liq = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_liq_h2
      endif
      call gstmed(clas_med_h2liq,'H2liq$',          mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c
      clas_med_d2liq = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_liq_d2
      endif
      call gstmed(clas_med_d2liq,'D2liq$',          mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c
c clas ch2
      clas_med_ch2=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_ch2
        endif 
        call gstmed(clas_med_ch2,'CH2       $',    mate,
     1         1,init_ifield,init_fieldm,
     1         0.5, 0.5, 0.05,0.005,0.003,0, 0)
c
c clas tio2
c 
      clas_med_tio2=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_tio2
        endif 
        call gstmed(clas_med_tio2,'TIO2       $',    mate,
     1         1,init_ifield,init_fieldm,
     1         0.5, 0.5, 0.05,0.005,0.003,0, 0)
c
c clas polistyrene
c
        clas_med_polistyrene=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_polystyrene
        endif 
           call gstmed(clas_med_polistyrene,'MED_POL   $',mate,
     1         1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
c                                
c clas  aluminized mylar
        clas_med_alumylar=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_alumylar
        endif 
           call gstmed(clas_med_alumylar,'MED_AMYLAR   $',mate,
     1         1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
                                
c clas  aluminized mylar & cerex
        clas_med_cerexmyl1=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_cerexmyl1
        endif 
           call gstmed(clas_med_cerexmyl1,'CEREX/ALUM.MYL 25/22$',mate,
     1         1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
     
c clas  composition alumaseal
        clas_med_alumaseal=get_tmed()
       if(nomate(other))then
        mate=16
        else
          mate=imate_comp_alumaseal
        endif 
           call gstmed(clas_med_alumaseal,'ALUMASEAL/KAPTON $',mate,
     1         1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
                               
c clas  low density foam for scattering chamber
      clas_med_lowdfoam = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_lowdfoam
      endif
      call gstmed(clas_med_lowdfoam,'ROHACELL$', mate,
     1     1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)

c clas carbon fiber/epoxy

      clas_med_cfiber = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_carb_fiber
      endif
      call gstmed(clas_med_cfiber,'CARB_FIBER$', mate,
     1     1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)

c clas cerex

      clas_med_cerex = get_tmed()
      if(nomate(other))then
         mate=16
      else
         mate=imate_cerex
      endif
      call gstmed(clas_med_cerex,'CEREX$', mate,
     1     1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
c clas tungsten
      clas_med_tungsten = get_tmed()
       if(nomate(other))then
         mate=16
       else
         mate=imate_tungsten
       endif
      call gstmed(clas_med_tungsten,'TUNGSTEN$', mate,
     1     1,init_ifield,init_fieldm,
     1     0.,   0., 0.05,   0.005,  0.003, 0, 0)
       return
       end

c--------------------------------------------------------------------









