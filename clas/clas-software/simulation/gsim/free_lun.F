	subroutine free_lun(unit)

c_begin_doc

c  Documentation for subroutine free_lun(unit)

c  $Id: free_lun.F,v 1.2 1997/04/24 15:38:29 holtrop Exp $

c  Purpose:
c  --------
c     frees already reserved (via get_lun) logical unit number 

c  Input:
c  ------
c     integer unit

c  Author:
c  -------
c     E.Wolin, W&M, 1-may-95

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: free_lun.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:29 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'FREE_LUN')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: free_lun.F,v 1.2 1997/04/24 15:38:29 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
	integer unit
      
c  local variables:
c  ----------------
	integer iret
c_end_var


c  executable code:
c  ----------------

c  use Arne's stuff
	call reslun(crname,-unit,iret)
	if(iret.ne.1)then
	   call recmes(crname,'I','?unable to free unit')
	endif

	return
	end

c---------------------------------------------------------------------------------


