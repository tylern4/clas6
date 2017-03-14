      subroutine ffky_st

c_begin_doc

c  Documentation for subroutine ffky_st

c  $Id: ffky_st.F,v 1.5 2004/09/07 16:46:09 devita Exp $

c  Purpose:
c  --------
c	ffkey defs for stc

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_st.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/09/07 16:46:09 $')
      parameter (cautho= '$Author: devita $')
      parameter (crname= 'ffky_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_st.F,v 1.5 2004/09/07 16:46:09 devita Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_st.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
      call ffkey('STCUTS',st_cuts,5,'REAL')
      st_cuts(1)=0.001
      st_cuts(2)=0.001
      st_cuts(3)=0.001
      st_cuts(4)=0.001
      st_cuts(5)=0.0001

c  z-offset wrt. nominal position of start counter
      call ffkey('STZOFF',st_zoff_ff,1,'REAL')
      st_zoff_ff = 0.0


c  st type, 0=old, 1=new 
      call ffkey('STTYPE',st_type,1,'REAL')
      st_type = 0.0

      return
      end


c---------------------------------------------------------------------------------

