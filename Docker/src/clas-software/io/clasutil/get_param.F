      subroutine get_param(cmd_flag,env_name,trans,iflag)

c_begin_doc

c  Documentation for subroutine get_param(cmd_flag,env_name,trans,iflag)

c  $Id: get_param.F,v 1.1.1.1 1997/03/28 20:50:00 marki Exp $

c  Purpose:
c  --------
c     Gets translation of parameter name from command line or env variable
c        first checks for command line flag
c        if none, checks for and translates env var

c  accepts command line format:   prog_name -flag1 param1 -flag2 param2 ...


c  Input:
c  ------
c     cmd_flag 		flag on command line (n.b. leave out the "-")
c     env_name          env variable name

c  Output:
c  -------
c     trans             translation of command line flag or env var
c     iflag             < 0   error...no file found
c                         1   found command line flag
c                         2   found env variable

c  Author:
c  -------
c     Elliott Wolin, W&M, 6-jul-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_param.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:50:00 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'GET_PARAM')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_param.F,v 1.1.1.1 1997/03/28 20:50:00 marki Exp $
     1 '/
      save


c  include files:
c  --------------
#include "slate.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      character*(*) cmd_flag,env_name,trans
      integer iflag
      
c  local variables:
c  ----------------
      integer lenocc,narg,iargc,iarg,find_arg
c_end_var


c  executable code:
c  ----------------

c  check for matching flag on command line
      narg=iargc()
      iarg=find_arg(cmd_flag)
      if((iarg.gt.0).and.(iarg.lt.narg))then
         call getarg(iarg+1,trans)
         if(trans(1:1).ne.'-')then
            iflag=1
            return
         endif
      endif


c  no command line flag found, check for environment variable
      if(lenocc(env_name).gt.0)then
         call getenvf(env_name,trans)
         if(islate(1).gt.0)then
            iflag=2
            return
         endif
      endif


c  neither found
      iflag=-1

      return
      end

c---------------------------------------------------------------------------------

