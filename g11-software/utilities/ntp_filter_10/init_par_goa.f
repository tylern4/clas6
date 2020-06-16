      SUBROUTINE init_par_goa(Nfiles,Fname,OutFile,OutFile2)      
      IMPLICIT NONE
      INCLUDE "ntpl_goa.inc"
      INCLUDE "photon.inc"
      INTEGER*4     istat,lrecl,ierr
      INTEGER*4     Nfiles,i,ichoice
      CHARACTER*200  Fname(999), OutFile,OutFile2
      DATA lrecl/8192/                !record lenght in machine words


c      ana_chan    = 0
c      filter_flag = .false.


c----------------------------------------------------------------------
c         Enter input information
c----------------------------------------------------------------------


c --- enter input file type ---
c         input_type=2
c        input_ntdata_num = 10
c        input_ntmc_num   =  9

        open (unit=61,file='inpfile',status='old',err=10)        
c --- read input files names ---
 1      continue
cx 1    WRITE (*,'(" Enter Ntuple ID you want to filter: ",$)')
        READ (61,*) ntupl_number
cx 1    WRITE (*,'(" Enter NUMBER of files you wish to analyze: ",$)')
        READ (61,*) Nfiles
c       Nfiles=1
c       write (*,*) 'Only 1 file mode'
c        num_files=Nfiles
cx      WRITE (*,'(" Enter INPUT file names one by one line: ")')
      do i=1,Nfiles
        READ  (61,'(a200)') Fname(i)
        Fname(i) = Fname(i)(1:index(Fname(i),' ')-1)
      enddo
      do i=1,Nfiles
        open (unit=1,file=Fname(i),status='old',err=2)
        goto 3 
  2     continue
        print *,'Input file ',Fname(i),' does NOT exist'
        goto 1
  3     continue
        close (1)  
      enddo
      write (*,'(" Input files are: ")')
      do i=1,Nfiles
        write (*,'(bna)') Fname(i)
      end do
      close (61)
 10    continue
c --- enter beam type ---
c      beam_type = .false.    ! (photon)




c --- set filter mode ---
cx      WRITE (*,'(" Enter filter mode ",$)')
cx      WRITE (*,'(" (0-nofilter, 1-filtered) ",$)')
cx      READ  (*,'(i2)') ichoice
       ichoice=1
      WRITE  (*,*)'Filtered mode'
c      if (ichoice.eq.1)  filter_flag= .true.



c --- read output files names ---      
c      WRITE (*,'(" Enter OUTPUT file name: ",$)')
c      READ  (*,'(a80)') OutFile
c      OutFile2= OutFile(1:index(OutFile,' ')-1)//'_files'
c      OutFile = OutFile(1:index(OutFile,' ')-1)//'.hbook'
       OutFile = 'spool.flt.hbook'
c      WRITE (*,'(" Output file name is   : ",$)')
c      WRITE (*,'(bna)') OutFile
c      WRITE (*,'(bna)') OutFile2


c --- set the number of events to process ---
cx      WRITE (*,'(" Enter number of events you want to procees (0 - all file): ",$)')
cx      READ  *, n_events
c        n_events=0
c      WRITE  (*,*)'All events'        
c      if(n_events.le.0) then
c        n_events=0
c        PRINT *,'Allora, facciamo tutto'
c      else
c        PRINT *,'Allora, facciamo solo ',n_events,' events'
c      endif


c----------------------------------------------------------------------
c              Initialize CERN "system" 
c----------------------------------------------------------------------

      call hlimit(128*maxpages)       !
      call hbset ('BSIZE',lrecl,ierr) !define buffersize for CWN
      !to increase max number of records in output hbook file to 64K
      IQUEST(10) = 65000


c Open output hbook file
      call hropen(2,'DST',OutFile,'NQ',lrecl,istat)


c call initialization routine for g1cPcor
      CALL InitPcor()
	print *,'Pcor configuration completed'
      
c call initialization routine for eloss
      CALL Initeloss(0)



      RETURN
      END 



