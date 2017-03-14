      subroutine init_sc

c_begin_doc

c  Documentation for subroutine init_sc

c  $Id: init_sc.F,v 1.3 1998/01/22 19:24:27 lcsmith Exp $

c  Purpose:
c  --------
c     Initialize sc package...after ffread but before geometry
c     Defines materials, media, etc.

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 16-oct-95
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_sc.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/01/22 19:24:27 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'init_sc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_sc.F,v 1.3 1998/01/22 19:24:27 lcsmith Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "scmate.inc"
#include "sctmed.inc"
#include "ffpar_sc.inc"
#include "ffpar_mg.inc"
c_end_inc

c  local variables:
c  ----------------

c  SC plastic scintillator
      integer mxscin
      parameter (mxscin=2)
      real ascin(mxscin),zscin(mxscin),wscin(mxscin),dnscin
      data  ascin,            zscin,        wscin,          dnscin
     1    /   12.,   1.0,    6.,    1.,    1.,    1.104,   1.035/

c  stainless steel
      integer mxstst
      parameter (mxstst=3)
      real astst(mxstst),zstst(mxstst),wstst(mxstst),dnstst
c                  Fe    Cr    Ni
c                  --    --    --
      data astst / 56. , 52. , 59. /
      data zstst / 26. , 24. , 28. /
      data wstst / 7.4 , 1.8 , 0.8 /
      data dnstst / 7.75 /

c  foam
      real afoam(4), zfoam(4), wfoam(4)
c            
c                  H     N     C     O
c                  -     -     -     -
      data afoam / 1. , 14. , 12. , 16. /
      data zfoam / 1. ,  7. ,  6. ,  8. /
      data wfoam / 7. ,  1.,   3. ,  2. /

      integer get_mate,get_tmed,mate

c  -------------------
c_end_var


c  executable code:
c  ----------------

c  SC mother volume air
      sc_med_air = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=15
      endif
      call gstmed(sc_med_air,'SC Air$', mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(sc_med_air,sc_cuts)
 

c  SC mother volume helium
      sc_mat_helium = get_mate()
      call gsmate(sc_mat_helium,'SC He gas$',
     +    4.00, 2.0, 17.8E-5, 5.3E5, 3.66E5, 0,0)

      sc_med_helium = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=sc_mat_helium
      endif
      call gstmed(sc_med_helium,'SC Helium$', mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(sc_med_helium,sc_cuts)
 

c  define SC plastic scintillator
      sc_mat_scint = get_mate()
      call gsmixt(sc_mat_scint,'SC scintillator$',
     +             ascin, zscin,dnscin,-mxscin, wscin)
      sc_med_scint = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=sc_mat_scint
      endif
      call gstmed(sc_med_scint,'SC Scintillator$', mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(sc_med_scint,sc_cuts)

c  SC lead in front of scintillator
      sc_med_lead = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=13
      endif
      call gstmed( sc_med_lead,'SC Lead$', mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .006,   .5,   0.006,  0.003, 0, 0)
      call set_tpar(sc_med_lead,sc_cuts)

c  define SC stainless backing support plates

      sc_mat_steel = get_mate()
      call gsmixt(sc_mat_steel,'SC St. steel$',
     +             astst, zstst,dnstst,-mxstst,wstst)
      sc_med_steel = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=sc_mat_steel
      endif
      call gstmed(sc_med_steel,'SC St. Steel$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .03,   .1,   0.06,  0.03, 0,0)
      call set_tpar(sc_med_steel,sc_cuts)
      
c  define SC foam      
      
      sc_mat_foam = get_mate()
      call gsmixt(sc_mat_foam,'SC Foam$',
     +             afoam, zfoam, 0.08 , -4, wfoam)

      sc_med_foam = get_tmed()
      if(nomate(sc))then
         mate=16
      else
         mate=sc_mat_foam
      endif
      call gstmed(sc_med_foam,'SC Foam$', mate,
     +     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(sc_med_foam,sc_cuts)

      return
      end

c---------------------------------------------------------------------------------

