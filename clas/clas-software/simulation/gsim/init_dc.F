      subroutine init_dc

c_begin_doc

c  Documentation for subroutine init_dc

c  $Id: init_dc.F,v 1.4 2001/04/23 17:53:14 fklein Exp $

c  Purpose:
c  --------
c	initialize ch package...after ffread but before geometry

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_dc.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/04/23 17:53:14 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'init_dc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_dc.F,v 1.4 2001/04/23 17:53:14 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_mg.inc"
#include "ffpar_dc.inc"
#include "clmate.inc"
#include "dcmate.inc"
#include "dctmed.inc"
c_end_inc

c  local variables:
c  ----------------

c  define chamber gas: argon/ethan 50:50
      integer mxaret
      parameter (mxaret = 3)
      real aaret(mxaret), zaret(mxaret), waret(mxaret)
      real denar,denet
      data      aaret / 39.95, 12.01,  1.01/
      data      zaret / 18.00,  6.00,  1.00/
      data      waret /  1.00,  2.00,  6.00/
      data      denar /  1.78e-3/
      data      denet /  1.50e-3/

*  (11)  CO_2 mixture parameters
      REAL  A_CO2(2), Z_CO2(2), W_CO2(2), dens_CO2
      DATA A_CO2 / 12.01, 16.00 /
      DATA Z_CO2 /  6.0,   8.0 /
      DATA W_CO2 /  1.0,   2.0 /
      DATA dens_CO2 / 0.001977 /
*
*  (12)  Isobuthane mixture parameters
      REAL  A_iso(2), Z_iso(2), W_iso(2), dens_iso
      DATA  A_iso /12.01, 1.01 /
      DATA  Z_iso /  6.0, 1.0  /
      DATA  W_iso /  4.0, 10.0 /
      DATA  dens_iso / 0.00267 /
*
*  (13)  Ethane mixture parameters
      REAL  A_eth(2), Z_eth(2), W_eth(2), dens_eth
      DATA  A_eth / 12.01,  1.01/
      DATA  Z_eth /  6.0,   1.0 /
      DATA  W_eth /  2.0,   6.0 /
      DATA  dens_eth / 0.00150 /
*
*  parameter for Argon, Helium, Neon
      REAL  A_arg, Z_arg, dens_arg
      DATA  A_arg, Z_arg, dens_arg / 39.95, 18.0, 0.00178 /
      REAL  A_hel, Z_hel, dens_hel
      DATA  A_hel, Z_hel, dens_hel / 4.00, 2.0, 0.000178 /
      REAL  A_neon, Z_neon, dens_neon
      DATA  A_neon, Z_neon, dens_neon / 20.18, 10.0, 0.00090 /

      integer mate,get_mate,get_tmed
      real density, dens, A_gas(3), Z_gas(3), W_gas(3)
      character*14 mat_name
c_end_var


c  executable code:
c  ----------------

c  define DC materials--------------------------------------------
      dc_mat_dcgas = get_mate()

      A_gas(1)=A_arg
      A_gas(2)=A_CO2(1)
      A_gas(3)=A_CO2(2)
      Z_gas(1)=Z_arg
      Z_gas(2)=Z_CO2(1)
      Z_gas(3)=Z_CO2(2)
      W_gas(1)=0.90
      W_gas(2)=0.10*W_CO2(1)
      W_gas(3)=0.10*W_CO2(2)
      dens=0.90*dens_arg + 0.10*dens_CO2
      mat_name='ARG/CO2 90:10$'
      CALL GSMIXT(dc_mat_dcgas,mat_name, A_gas,Z_gas, dens,-3,W_gas)

c  dc gas is 50-50 Ar/Ethane
c      density = 0.5 * denar + 0.5 * denet
c      call gsmixt(dc_mat_dcgas,'DC gas 50-50 Ar/Ethane$',
c     +             aaret,zaret,density,-mxaret,waret)


c  define DC tracking media--------------------------------------

c  dc air
      dc_med_air = get_tmed()
      if(nomate(dc))then
         mate=16
      else
         mate=15
      endif
      call gstmed(dc_med_air,'DC Air$',mate,
     1     1     ,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
      call set_tpar(dc_med_air,dc_cuts)


c  dc helium 
      dc_med_helium = get_tmed()
      if(nomate(dc))then
         mate=16
      else
         mate=imate_helium
      endif
      call gstmed(dc_med_helium,'DC He$',mate,
     1     1,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
      call set_tpar(dc_med_helium,dc_cuts)


c  dc chamber gas
      dc_med_dcgas   = get_tmed()
      if(nomate(dc))then
         mate=16
      else
         mate=dc_mat_dcgas
      endif
c      call gstmed(dc_med_dcgas,'DC gas 50-50 Ar/Ethane$',mate,
c     +     1,init_ifield,init_fieldm,
c     1     0.3,   0.5,   .05,   0.005,  0.003, 0, 0)
      call gstmed(dc_med_dcgas,mat_name,mate,
     +     1,init_ifield,init_fieldm,
     1     0.3,   0.5,   .05,   0.005,  0.003, 0, 0)
      call set_tpar(dc_med_dcgas,dc_cuts)

      return
      end

c---------------------------------------------------------------------------------

