      subroutine init_st

c_begin_doc

c  Documentation for subroutine init_st

c  $Id: init_st.F,v 1.2 1997/04/24 15:38:45 holtrop Exp $

c  Purpose:
c  --------
c     Initialize st package...after ffread but before geometry
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
      parameter (cfile=  '$RCSfile: init_st.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:45 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'init_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_st.F,v 1.2 1997/04/24 15:38:45 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "stmate.inc"
#include "sttmed.inc"
#include "ffpar_st.inc"
#include "ffpar_mg.inc"
c_end_inc

c  local variables:
c  ----------------

c  ST plastic scintillator
      integer mxscin
      parameter (mxscin=2)
      real ascin(mxscin),zscin(mxscin),wscin(mxscin),dnscin
      data  ascin,            zscin,        wscin,          dnscin
     1    /   12.,   1.0,    6.,    1.,    1.,    1.104,   1.035/

      integer get_mate,get_tmed,mate
c_end_var


c  executable code:
c  ----------------

c  ST plastic scintillator
      st_mat_scint = get_mate()
      call gsmixt(st_mat_scint,'ST scintillator$',
     +             ascin, zscin,dnscin,-mxscin, wscin)

      st_med_scint = get_tmed()
      if(nomate(st))then
         mate=16
      else
         mate=st_mat_scint
      endif
      call gstmed(st_med_scint,'ST Scintillator$', mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   .5,   .05,   0.1,  0.1, 0, 0)
      call set_tpar(st_med_scint,st_cuts)
      
      return
      end

c---------------------------------------------------------------------------------

