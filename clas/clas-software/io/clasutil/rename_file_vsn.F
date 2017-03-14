      subroutine rename_file_vsn(filename,lerr)

c_begin_doc

c  Documentation for subroutine rename_file_vsn(filename,lerr)

c  $Id: rename_file_vsn.F,v 1.1.1.1 1997/03/28 20:50:02 marki Exp $

c  Purpose:
c  --------
c     Renames file by appending a version number
c     Does nothing if file does not exist

c  Notes:
c  ------
c     Appends version numbers in sequence; largest vsn is newest
c     Searches for highest version number
c     Uses GNU style version numbers, e.g:  filename.~1~, filename.~2~, etc.

c  Input:
c  ------
c     filename to rename

c  Output:
c  -------
c     lerr true on any error

c  Author:
c  -------
c    Elliott Wolin, W&M, 12-jul-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: rename_file_vsn.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:50:02 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'RENAME_FILE_VSN')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: rename_file_vsn.F,v 1.1.1.1 1997/03/28 20:50:02 marki Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      character*(*) filename
      logical lerr
      
c  local variables:
c  ----------------
      character*212 newname
      character*250 temp
      character*10 cvsn
      logical accessf,exists
      integer vsn,lenocc,flen,nlen,renamef,icfnbl,first_char,len
c_end_var


c  executable code:
c  ----------------

      lerr=.false.

c  do nothing if file does not exist
      if(.not.accessf(filename,0))return

c  err if file name too long
      if(lenocc(filename).gt.200)then
         lerr=.true.
         call recmes(crname,'I','?filename greater than 200 characters')
         call recmes(crname,'I',filename)
         return
      endif

c  check for highest version number for this file
      flen=lenocc(filename)
      vsn=0
      exists=.true.
      do while(exists)
         vsn=vsn+1
         write(cvsn,'(i10)')vsn
         first_char=icfnbl(cvsn,1,len(cvsn))
         newname=filename(1:flen) //'.~'//
     1        cvsn(first_char:len(cvsn))//'~'
         exists=accessf(newname,0)
      enddo

c  found unused version number...rename file
c  temp variable needed to get around IBM bug...
      nlen=lenocc(newname)
      temp=
     1     'renaming ' // filename(1:flen) // ' to ' //
     1     newname(1:nlen)
      call recmes(crname,'I',temp)

      if(renamef(filename,newname).ne.0)then
         lerr=.true.
         call recmes(crname,'I','?unable to rename file')
         return
      endif

      return
      end

c---------------------------------------------------------------------------------

