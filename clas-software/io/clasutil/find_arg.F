      integer function find_arg(arg)

c_begin_doc

c  Documentation for integer function find_arg(arg)

c  $Id: find_arg.F,v 1.1.1.1 1997/03/28 20:49:59 marki Exp $

c  Purpose:
c  --------
c     searchs command line for arg, returns index, assumes leading "-"

c  Input:
c  ------
c     arg      arg to search for, assumes leading "-"

c  Output:
c  -------
c     return value is index of arg, 0 if not found

c  Author:
c  -------
c     Elliott Wolin, W&M, 7-jul-95

c  Major revisions:
c  ----------------
c   Arne Freyberg truncated string comparison to lenocc(arg)
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: find_arg.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:49:59 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'FIND_ARG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: find_arg.F,v 1.1.1.1 1997/03/28 20:49:59 marki Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      character*(*) arg
      
c  local variables:
c  ----------------
      integer lenocc,arg_len,narg,iarg,iargc
      character*100 cmd_arg,temp
c_end_var


c  executable code:
c  ----------------

      find_arg=0

c  check for matching arg on command line
      narg=iargc()
      arg_len=lenocc(arg)
      if((narg.gt.0).and.(arg_len.gt.0))then

         do iarg=1,narg
            call getarg(iarg,cmd_arg)

            temp='-'//arg(1:arg_len)

c  add (1:arg_len+1) to the following test, this allows for trucation of
c  command line arguments
            if(cmd_arg(1:arg_len+1).eq.temp(1:lenocc(temp)))then
               find_arg=iarg
               return
            endif

         enddo

      endif

      return
      end

c---------------------------------------------------------------------------------

