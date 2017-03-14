      subroutine init_ec

c_begin_doc

c  Documentation for subroutine init_ec

c  $Id: init_ec.F,v 1.5 1998/11/28 17:58:15 boiarino Exp $

c  Purpose:
c  --------
c     initialize ec package...after ffread but before geometry
c     initializes materials, media, atten lens

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
c  ----------------
c
c    D. Tedeschi  add ec to cf list if fast code wanted
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_ec.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/11/28 17:58:15 $')
      parameter (cautho= '$Author: boiarino $')
      parameter (crname= 'INIT_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_ec.F,v 1.5 1998/11/28 17:58:15 boiarino Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_ec.inc"
#include "ffpar_mg.inc"
#include "ecmate.inc"
#include "ectmed.inc"
c_end_inc

c  local variables:
c  ----------------

c  scintillator
      real ascin(2),zscin(2),wscin(2),dnscin
      data    ascin,       zscin,         wscin,       dnscin
     1    /12.,   1.0,    6.,    1.,    1.,    1.104,   1.035/

c  stainless steel
      real astst(3), zstst(3), wstst(3)
c                  Fe    Cr    Ni
c                  --    --    --
      data astst / 56. , 52. , 59. /
      data zstst / 26. , 24. , 28. /
      data wstst / 7.4 , 1.8 , 0.8 /
      
c  foam
      real afoam(4), zfoam(4), wfoam(4)
c            
c                  H     N     C     O
c                  -     -     -     -
      data afoam / 1. , 14. , 12. , 16. /
      data zfoam / 1. ,  7. ,  6. ,  8. /
      data wfoam / 7. ,  1.,   3. ,  2. /
      
      integer get_mate,get_tmed,mate

      character*13 val

c_end_var


c  executable code:
c  ----------------

c  ec materials---------------------------------------------
      ec_mat_helium = get_mate()
      call gsmate(ec_mat_helium, 'EC Helium$',
     +             4.00, 2.0, 17.8E-5, 5.3E5, 3.66E5, 0,0)

      ec_mat_scint = get_mate()
      call gsmixt(ec_mat_scint,'EC scint$',
     +             ascin, zscin,dnscin,-2, wscin)

      ec_mat_steel = get_mate()
      call gsmixt(ec_mat_steel,'EC St. steel$',
     +             astst, zstst, 7.75 , -3, wstst)

      ec_mat_foam = get_mate()
      call gsmixt(ec_mat_foam,'EC Foam$',
     +             afoam, zfoam, 0.24 , -4, wfoam)


c  ec tracking media----------------------------------------

c  ec vacuum
      ec_med_vac=get_tmed()
         mate=16
      call gstmed(ec_med_vac,'EC Vacuum$',mate,
     1        0     ,init_ifield,init_fieldm,
     1     5.,   1.,   .1,   0.1,  0.1, 0, 0)


c  ec air
      ec_med_air=get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=15
      endif
      call gstmed(ec_med_air,'EC Air$',mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ec_med_air,ec_cuts)


c  ec cover plate air
      ec_med_cvp_air=get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=15
      endif
      call gstmed(ec_med_cvp_air,'EC cover plate air$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ec_med_cvp_air,ec_cuts)


c  ec helium
      ec_med_helium=get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=ec_mat_helium
      endif
      call gstmed(ec_med_helium,'EC Helium$',mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ec_med_helium,ec_cuts)


c  ec lead
      ec_med_lead = get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=13
      endif
      call gstmed( ec_med_lead,'EC Lead$', mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .1,   .05,   0.1,  0.05, 0, 0)
      call set_tpar(ec_med_lead,ec_cuts)


c  ec stainless steel
      ec_med_steel = get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=ec_mat_steel
      endif
      call gstmed(ec_med_steel,'EC St. Steel$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .1,   .05,   0.1,  0.05, 0, 0)
      call set_tpar(ec_med_steel,ec_cuts)


c  ec scintillator
      ec_med_scint = get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=ec_mat_scint
      endif
      call gstmed(ec_med_scint,'EC Scintillator$', mate,
     +     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(ec_med_scint,ec_cuts)
      
c  ec foam
      ec_med_foam = get_tmed()
      if(nomate(ec))then
         mate=16
      else
         mate=ec_mat_foam
      endif
      call gstmed(ec_med_foam,'EC Foam$', mate,
     +     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(ec_med_foam,ec_cuts)

c DJT
c put tracking media on cf list for fast shower
c
      if (fast_code(ec)) then
        call cfblkd
        call cfstmd(ec_med_vac)
        call cfstmd(ec_med_air)
        call cfstmd(ec_med_cvp_air)
        call cfstmd(ec_med_helium)
        call cfstmd(ec_med_lead)
        call cfstmd(ec_med_steel)
        call cfstmd(ec_med_scint)
        call cfstmd(ec_med_foam)
        call recmes(crname,'I','EC media added to CF list')
        call cfinit
      endif

      return
      end

c------------------------------------------------------------------------------

