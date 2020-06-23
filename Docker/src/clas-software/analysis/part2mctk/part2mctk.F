      program smearbos
      implicit none
c     part2mctk is a simple program which converts PART bank input into
c     MCTK and MCVX so that detector simulation software does not need
c     special flags.
c     
c     part2mctk needs to be built with the CLAS Make System (CMS), so it
c     should be built on JLAB computers unless one installs all relevant
c     JLAB software locally.

#include "bcs.inc"

      character * (100) infilename
      character * (100) outfilename
      integer nargs
      integer ierr

      integer convert
      
      character*132 ddl_file

      integer jw(Nbcs)
      real rjw(Nbcs)
      equivalence (jw(1),rjw(1))
      integer i, j

c$$$      real mp, mn, md
c$$$      parameter (mp=0.9383,mn=0.9396,md=1.8756)

c$$$      real target(4), spectator(4)
c$$$      integer target_type
c$$$      integer spectator_p
c$$$
c$$$      real beam                 ! beam energy in GeV

      i = 0
      j = 0

c     Initialize RNG, program start time should be good enough for this
c     Monte Carlo
      call srand(ieor(time(),int(getpid())))

c     Handle arguments
      nargs = command_argument_count()
      if(nargs .ne. 2) then
         write(0,*) "Usage: part2mctk <infile> <outfile>"
         write(0,*) ""
         write(0,*) "Converts PART bank to MCTK + MCVX"
         goto 1
      endif

c     Parse arguments
      call getarg(1,infilename)
      call getarg(2,outfilename)
c     Initialize BOS
      call bnames(1000)
      call bos(iw,nbcs)
      call bos(jw,nbcs)
      call revinm('CLAS_PARMS','clasbanks.ddl',ddl_file)
      call txt2bos(ddl_file)

      write (*,*) ''
      write(*,*) infilename
      write (*,*) ''
      write (*,*) 'OPEN bosinput UNIT=33' //
     &     ' FILE="'//infilename//'" ACTION=READ'
      write (*,*) ''
      write (*,*) infilename
      write (*,*) ''
      
c     Open files
      call fparm('OPEN bosinput UNIT=33' //
     &     ' FILE="'//infilename//'" ACTION=READ')

      call fparm('OPEN bosoutput UNIT=34' //
     &     ' FILE="'//outfilename//
     &     '" RECL=32760 STATUS=NEW ACTION=WRITE FORM=BINARY')

c     set output banks:
      call blist(jw,'E=','HEADMCTKMCVX')

c     Loop over all banks

c     call fparm('FILECOPY bosinput TO bosoutput ALL')
 2    call frbos(iw,33,'E',ierr)
      if(ierr.eq.-1) goto 3     ! end of file
c     Begin Event Loop
      ierr = convert(jw,rjw)
c     End Event Loop
      call fwbos(jw,34,'E',ierr)
      call bdrop(iw,'E')
      call bgarb(iw)
      call bdrop(jw,'E')
      call bgarb(jw)
      goto 2
c     Close all files
 3    call fparm('CLOSE bosinput')

      call bosta

      call fwbos(jw,34,'0',ierr)
      call fparm('CLOSE bosoutput UNIT=34 STATUS=KEEP')

 1    stop
      end
