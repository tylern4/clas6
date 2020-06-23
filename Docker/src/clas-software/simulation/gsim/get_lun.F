      integer function get_lun()

c_begin_doc

c  Documentation for integer function get_lun()

c  $Id: get_lun.F,v 1.2 1997/04/24 15:38:34 holtrop Exp $

c  Purpose:
c  --------
c     gets an unused fortran logical unit number

c  Notes:
c  ------
c     returns -1 if no free unit available

c  Author:
c  -------
c     E.Wolin, W&M, 1-may-95

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_lun.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:34 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GET_LUN')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_lun.F,v 1.2 1997/04/24 15:38:34 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  local variables:
c  ----------------
	integer iret
c_end_var


c  executable code:
c  ----------------

c  use Arne's reslun facility to get logical units
        get_lun=0
        call reslun(crname,get_lun,iret)
        if(iret.ne.1)then
           call recmes(crname,'=F',
     1          '?unable to reserve logical unit number')
        endif

	return
	end

c---------------------------------------------------------------------------------


