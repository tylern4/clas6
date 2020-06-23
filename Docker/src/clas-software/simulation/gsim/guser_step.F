	subroutine guser_step

c_begin_doc

c  Documentation for subroutine guser_step

c  $Id: guser_step.F,v 1.4 2006/08/30 20:14:12 holtrop Exp $

c  Purpose:
c  --------
c       called after each tracking step

c  Methods:
c  --------

c  Input:
c  ------

c  Output:
c  -------

c  Controls:
c  ---------

c  Other routines:
c  ---------------
c       called by gustep

c  Notes:
c  ------

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_step.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2006/08/30 20:14:12 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_STEP')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_step.F,v 1.4 2006/08/30 20:14:12 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gccuts.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gcking.inc"
#include "gclist.inc"
#include "gcmate.inc"
#include "gcnum.inc"
#include "gcphys.inc"
#include "gcsets.inc"
#include "gcstak.inc"
#include "gctmed.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
	logical first_time/.true./
c_end_var


c  executable code:
c  ----------------

	if(first_time)then
		first_time=.false.
		call recmes(crname,'I',
     1  	     ' *** Default version of guser_step called ***')
	endif

	return
	end

c------------------------------------------------------------------------
