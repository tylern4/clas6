      subroutine bos_mcpa

c_begin_doc

c  Documentation for subroutine bos_mcpa

c  $Id: bos_mcpa.F,v 1.2 1997/04/24 15:38:03 holtrop Exp $

c  Purpose:
c  --------
c	Fills MCPA banks with begin run information

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 24-aug-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_mcpa.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:03 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'BOS_MCPA')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_mcpa.F,v 1.2 1997/04/24 15:38:03 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
c_end_inc

c  local variables:
c   ----------------
      integer nbank,mcpa
c_end_var


c  executable code:
c  ----------------

      mcpa=nbank('MCPA',0,1)
      iw(mcpa+ 1) = 1              !version of mcpa format

      return
      end

c------------------------------------------------------------------------

