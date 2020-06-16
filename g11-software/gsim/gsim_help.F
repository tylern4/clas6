      subroutine gsim_help

c_begin_doc

c  Documentation for subroutine gsim_help

c  $Id: gsim_help.F,v 1.2 1997/04/24 15:38:36 holtrop Exp $

c  Purpose:
c  --------
c     prints help text

c  Author:
c  -------
c     Elliott Wolin, W&M, 7-jul-95

c  Major revisions:
c  ----------------
c     Switched from format statements to external file, ejw, W&M, 11-jul-95
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: gsim_help.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:36 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GSIM_HELP')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gsim_help.F,v 1.2 1997/04/24 15:38:36 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      
c  local variables:
c  ----------------
      integer istat,systemf,lenocc
      character*120 helpfilename
c_end_var


c  executable code:
c  ----------------

      call revinm(clas_doc,gsim_help_filename,helpfilename)
      
      write(*,*)
      istat=systemf('cat '//helpfilename)
      write(*,*)
      write(*,*)
      write(*,*)'(This is file: ' //
     1     helpfilename(1:lenocc(helpfilename)) // ')'
      write(*,*)


      return
      end

c---------------------------------------------------------------------------------

