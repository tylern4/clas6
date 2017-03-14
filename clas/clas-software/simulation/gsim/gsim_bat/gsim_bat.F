      program gsim_bat

c_begin_doc

c  Documentation for program gsim_bat

c  Purpose:
c  --------
c     example main program for geant batch simulation:
c     defines max allowed job time
c     disables/enables log file
c     defines size of zebra and hbook commons
c     initialized bos 
c     calls main steering routines
c     declares signal handler for SIGTERM to cause graceful exit

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 23-jun-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*128 rcsid
      data rcsid/'
     1$Id: gsim_bat.F,v 1.11 2008/10/03 13:58:41 avakian Exp $
     1'/
      save

c  include files:
c  --------------
#include <gcnum.inc>
#include <gcflag.inc>
c_end_inc

C      COMMON/GCFLAX/BATCH, NOLOG
C      LOGICAL BATCH, NOLOG

c  local variables:
c  ----------------
      real max_time
      parameter (max_time=604800.)           !max allowed job time, in seconds

      integer max_zebra,max_hbook
      parameter (max_zebra=7500000)         !zebra common block size
      parameter (max_hbook=200000)          !hbook common block size

      common/gcbank/q(max_zebra)
      common/pawc/h(max_hbook)
      real q,h

      integer find_arg,error

      integer signal
      external sigterm_handler, signal
c_end_var


c  executable code:
c  ----------------

c  generate help message
      if((find_arg('h').gt.0).or.(find_arg('help').gt.0))then
         call gsim_help
         stop
      endif

c  declare signal handler for SIGTERM (15)
      error=signal(15,sigterm_handler,-1)
      if(error.lt.0)then
         write (*,*)
         write (*,*)'?gsim_bat...unable to declare signal handler!'
         write (*,*)
      endif

c  set BATCH flag off, turn on the logging. This seems to give the
c  messages to STDOUT and not make a re00000.log file.

      BATCH=.false.
      call set_log(.true.)


c  specify max allowed job time...this may be system specific, or not even needed
      call timest(max_time)

#ifdef DEBUG
      call clas_enable_fp_trap
#endif

c  allocate memory for zebra and hbook...allocate more if needed
      call gzebra( max_zebra)
      call hlimit(-max_hbook)           !n.b. minus sign is critical!

c  initialize geant, particles, geometry, hist, etc.
      call uginit

c  start processing events
      call grun

c  done
      call uglast

      end

c------------------------------------------------------------------------------

      subroutine never_gets_called
      
c  this is needed to force linking of following globals:

      call uginit
      call uglast
      call gutrev
      call gutrak
      call gukine
      call gustep
      call guskip
      call guout
      call gudigi
      call gufld
      call gtnino

      return
      end

c-----------------------------------------------------------------------------

      subroutine gdcxyz

c  to avoid loading graphics routines in batch vsn
c  from the Geant manual

      entry igsa
      entry gdtrak

      return
      end

c-----------------------------------------------------------------------------

      integer function sigterm_handler()

c  gracefully ends run


#include "gcflag.inc"

      ieorun=1
      sigterm_handler=0

      return
      end

c-----------------------------------------------------------------------------
