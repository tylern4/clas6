      subroutine clas_ffgo

c_begin_doc

c  Documentation for subroutine clas_ffgo

c  $Id: clas_ffgo.F,v 1.4 2002/04/25 15:42:38 holtrop Exp $

c  Purpose:
c  --------
c     Reads FFREAD data cards from file or terminal

c  Author:
c  -------
c     Elliott Wolin, W&M, 10-jul-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_ffgo.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2002/04/25 15:42:38 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'CLAS_FFGO')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_ffgo.F,v 1.4 2002/04/25 15:42:38 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcflag.inc"
#include "gctrak.inc"
#include "ffpar.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      
c  local variables:
c  ----------------
      character*120 filename
      integer unit,iret,find_arg,iflag
      integer iunixtime,idigit
      
c_end_var


c  executable code:
c  ----------------

c  is ffread vetoed on command line
      noffread=find_arg(gsim_noffread_flag).ne.0

c  check for veto on command line
      if(noffread)then
         call recmes(crname,'I',
     1        '"'//gsim_noffread_flag//'"'//
     1        ' found on command line...FFREAD cards NOT read in!')
         return
         endif

c  no veto...read in ffread cards
      call recmes(crname,'I','Processing FFREAD data cards')

c  check for flag to read ffread cards from file
      call get_param(gsim_ffread_flag,gsim_ffread_env,filename,iflag)

c  no file...read from terminal
      if(iflag.lt.0)then
         call recmes(crname,'I','enter FFREAD data cards now:')
         call gffgo

c  filename specified on command line or with env variable
      else
         if(iflag.eq.1)then
            call recmes(crname,'I',
     1           'getting FFREAD filename from command line flag: -' //
     1           gsim_ffread_flag)
         else
            call recmes(crname,'I',
     1           'getting FFREAD file name from env variable: ' //
     1           gsim_ffread_env)
         endif
         call reslun(crname,unit,iret)
         if(iret.eq.-1)call recmes(crname,'F','?unable to reserve unit')
         call reopen(filename,unit,'old',iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to open ffread file')
         call ffset('LINP',unit)
         call gffgo
         close(unit=unit)
         call reslun(crname,-unit,iret)
      endif

      call recmes(crname,'I','FFREAD data cards read in')

C
C In the "gffgo" call the RNMD card is read from the FFREAD list, and the random
C generator is initialized. If no RNDM cards were specified, GEANT uses the standard
C 215 sequence, starting with sequence 0.
C Here we modify this behavior by testing for RNMD = 0,0 in which case we initialize
C the random generator with the "unixtime" = # of seconds since 00:00 January 1, 1970.
C We use the last digit to make sure BOTH random numbers will be different from second to second.
C Right now this is  898028275 => iseed2=8980+5  iseed1=2827+5
C
C      WRITE(6,*)'Random flags: ',NRNDM(1),NRNDM(2)
      if(NRNDM(1).EQ.0)then
         call getunixtime(iunixtime)
         idigit = mod(iunixtime,10)
         NRNDM(1) = iunixtime/100000+idigit
         NRNDM(2) = mod(iunixtime/10,10000)+idigit
         
         WRITE(6,*)' Initializing the Random Generator to ',NRNDM(1),NRNDM(2)
         CALL GRNDMQ(NRNDM(1),NRNDM(2),1,'S')
      endif

C
C We need to propagate the NSTEPMAX to the appropriate variable in the GEANT banks:
C gctrak.MAXNST
C
      MAXNST = nstepmax_ff

      return
      end

c---------------------------------------------------------------------------------

