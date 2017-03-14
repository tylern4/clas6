      subroutine uglast

c_begin_doc

c  Documentation for subroutine uglast

c  $Id: uglast.F,v 1.2 1997/04/24 15:38:58 holtrop Exp $

c  Purpose:
c  --------
c     Geant standard termination routine

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: uglast.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:58 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'UGLAST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: uglast.F,v 1.2 1997/04/24 15:38:58 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gcflag.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gsimout.inc"
c_end_inc

c  local variables:
c  ----------------
      integer ierr,iky,mdrop
      character*100 crmess
c_end_var


c  executable code:
c  ----------------

c  Geant standard termination routine
      call glast

c  user-specific termination
      call clas_last
      call guser_last

c  write end run record and close BOS event output file
      if(.not.nobosout(all))then

c  EOR record
         iky=mdrop(iw,'+KYE',0)
         call bos_head(20,idrun,0)     !class 20 is EOR, event number is 0
         call bos_out
         call bdrop(iw,'E')
         call bgarb(iw)

c  close file
         call fwbos(iw,gsimout_unit,'0',ierr)
         if(ierr.ne.0)then
            write(crmess,
     1           '(''?unable to write last bos record, ierr is '',i4)')
     1           ierr
            call recmes(crname,'I',crmess)
         endif
         call fparm('CLOSE '//gsimout_daname)
         call recmes(crname,'I',
     1        'closing BOS output file stream '//gsimout_daname)
      endif

c  force Arne's message system to dump the last few messages in memory
      call recmes(crname,'S',' ')
      
      return
      end

c------------------------------------------------------------------------
