      subroutine init_ec1

c_begin_doc

c  Documentation for subroutine init_ec1

c  $Id: init_ec1.F,v 1.2 1997/04/24 15:38:44 holtrop Exp $

c  Purpose:
c  --------
c	initialize ec1 package...after ffread but before geometry

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
c  Harout Avakian  5 Jan 1995
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_ec1.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:44 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'init_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_ec1.F,v 1.2 1997/04/24 15:38:44 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_ec1.inc"
#include "ec1tmed.inc"
#include "ec1mate.inc"
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

      integer get_mate,get_tmed,mate
      integer init_ifield
      real init_fieldm
c_end_var


c  executable code:
c  ----------------

      init_fieldm=0.0
      init_ifield=0
c  ec1 materials---------------------------------------------
      ec1_mat_scint = get_mate()
      call gsmixt(ec1_mat_scint,'EC1 scint$',
     +             ascin, zscin,dnscin,-2, wscin)

      ec1_mat_steel = get_mate()
      call gsmixt(ec1_mat_steel,'EC1 St. steel$',
     +             astst, zstst, 7.75 , -3, wstst)



c  ec1 tracking media----------------------------------------

c  ec1 vacuum
      ec1_med_vac=get_tmed()
         mate=16
      call gstmed(ec1_med_vac,'EC1 Vacuum$',mate,
     1        0     ,init_ifield,init_fieldm,
     1     5.,   1.,   .1,   0.1,  0.1, 0, 0)


c  ec1 air
      ec1_med_air=get_tmed()
      if(nomate(ec1))then
         mate=16
      else
         mate=15
      endif
      call gstmed(ec1_med_air,'EC1 Air$',mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ec1_med_air,ec1_cuts)


c  ec1 cover plate air
      ec1_med_cvp_air=get_tmed()
      if(nomate(ec1))then
         mate=16
      else
         mate=15
      endif
      call gstmed(ec1_med_cvp_air,'EC1 cover plate air$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ec1_med_cvp_air,ec1_cuts)


c  ec1 lead
      ec1_med_lead = get_tmed()
      if(nomate(ec1))then
         mate=16
      else
         mate=13
      endif
      call gstmed( ec1_med_lead,'EC1 Lead$', mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .1,   .05,   0.1,  0.05, 0, 0)
      call set_tpar(ec1_med_lead,ec1_cuts)


c  ec1 stainless steel
      ec1_med_steel = get_tmed()
      if(nomate(ec1))then
         mate=16
      else
         mate=ec1_mat_steel
      endif
      call gstmed(ec1_med_steel,'EC1 St. Steel$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .1,   .05,   0.1,  0.05, 0, 0)
      call set_tpar(ec1_med_steel,ec1_cuts)


c  ec1 scintillator
      ec1_med_scint = get_tmed()
      if(nomate(ec1))then
         mate=16
      else
         mate=ec1_mat_scint
      endif
      call gstmed(ec1_med_scint,'EC1 Scintillator$', mate,
     +     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(ec1_med_scint,ec1_cuts)
      return
      end

c---------------------------------------------------------------------------------

