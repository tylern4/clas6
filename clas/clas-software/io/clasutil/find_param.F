      logical function find_param(cmd_flag,env_name)

c_begin_doc

c  Documentation for subroutine find_param(cmd_flag,env_name)

c  $Id: find_param.F,v 1.1.1.1 1997/03/28 20:49:59 marki Exp $

c  Purpose:
c  --------
c     checks if command line flag or env variable exists
c     returns true if either found, false otherwise

c  Input:
c  ------
c     cmd_flag 		flag on command line (n.b. leave out the "-")
c     env_name          env variable name

c  Output:
c  -------
c     function returns true if translation found

c  Author:
c  -------
c     Elliott Wolin, W&M, 13-jul-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: find_param.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:49:59 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'FIND_PARAM')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: find_param.F,v 1.1.1.1 1997/03/28 20:49:59 marki Exp $
     1 '/
      save


c  include files:
c  --------------
#include "slate.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      character*(*) cmd_flag,env_name
      
c  local variables:
c  ----------------
      character*80 trans
      integer lenocc,find_arg
c_end_var


c  executable code:
c  ----------------

      find_param=.true.

c  check for flag on command line
      if(find_arg(cmd_flag).gt.0)return

c  no command line flag found, check for environment variable
      if(lenocc(env_name).gt.0)then
         call getenvf(env_name,trans)
         if(islate(1).gt.0)return
      endif

c  neither found
      find_param=.false.

      return
      end

c---------------------------------------------------------------------------------

