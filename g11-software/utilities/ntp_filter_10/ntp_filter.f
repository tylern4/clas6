ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c To filter a new ntuple:
c - define the ntupl_number (<> 92 and 93) default=60
c - create the include file usinng uwfunc and save as ntpl_structure.inc
c - change filter.f
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      PROGRAM ntp_filter
      IMPLICIT NONE
      INCLUDE "ntpl_goa.inc"
      INCLUDE "ntpl_structure.inc"

      CHARACTER*200 Fname(999), file_in,OutFile,OutFile2
      INTEGER*4 ierr98,ierr99
      INTEGER*4 ifile,icycle
      INTEGER*4 nfiles
      INTEGER*4 nstory,nstory_MC, NDATA
      INTEGER*4     istat,ic,n_evt
      LOGICAL   firsttime,eof,filter_ok_data,filter_ok_MC,g11_norm_flag

c----------------------------------------------------------------------
c Initialize
c----------------------------------------------------------------------      

      CALL init_par_goa(Nfiles,Fname,OutFile,OutFile2)

      n_evt=1000000000
      filter_ok_data=.false.

      write(*,721)
  721 format(///'======================= START =======================')

      DO Ifile=1, Nfiles

c --- Some settings ---
c      I_file =Ifile
      file_in   =Fname(Ifile)
      file_input=Fname(Ifile)
c      N_firstfile1 = 0
      eof       = .false.
      firstTime = .true.
      Nstory    = 0
      ierr99 = 0
      ic=1
      write(*,720) Ifile,Nfiles,file_in
  720 format(/' Starting to Analyze data FILE (',I2,'of',I2,'): '/' ',A78)
      ic=1
      if (ifile.eq.1) ic=0

      CALL open_ntpl_goa(file_in,ic) ! first openining
     


c------------------------------------------------------------------- 
c  Data loop (ntuple 99/10)                  
c-------------------------------------------------------------------
      g11_norm_flag=.true.
      if(g11_norm_flag) then 
        CALL g11_norm
      endif

      print *,'PreProcessing DATA bank'
c --- main data loop ---
      print *,'Processing    DATA bank'
      DO WHILE (.true.)
	 Nstory =  Nstory + 1
         call HGNT(ntupl_number,Nstory,ierr99)
         IF(ierr99.ne.0) THEN
           Nstory = Nstory - 1
           GOTO 9000
         ENDIF	
          call filter(filter_ok_data)
	 if(filter_ok_data)	 CALL hfnt(93)
         if (Nstory.eq.n_evt) goto 9000
c-       
1010 	 continue  
c+ Writing events passed     
      if (Nstory/20000*20000.eq.Nstory.or.Nstory.eq.100) then
         write(*,*) Ifile,Nfiles,Nstory,NDATA,'DATA passed         '
      end if
      
      ENDDO  ! next event 

      
C----------------------------------------------------------
 9000   continue
	print *,'FILE completed. ',
     &          ' Events written to nt DATA/MC = ',Nstory,Nstory_MC

        print *,'Do hrend'
	CALL hrend('ESCA')
c        call hldir ('//PAWC','T')

 9001   continue

      ENDDO   ! next data file
      
C----------------------------------------------------------
	
c        CALL eff_pi_minus(0,0)     ! option 0 means final efficiency calculation

 9999 CONTINUE



        CALL hcdir('//DST',' ')
 	CALL hrout(0,icycle,' ')
	CALL hrend('DST')
ccccccccccccccccccccccccccccccc

c      CALL init_par_goa(Nfiles,Fname,OutFile,OutFile2)
      nfiles=1
      OutFile='data_filtered.hbook'
      call hropen(2,'PART',OutFile,'NQ',8192,istat)

c --- Some settings ---
c      I_file =1
      file_in   ='spool.flt.hbook'
      file_input=Fname(Ifile)
c      N_firstfile1 = 0
      eof       = .false.
      firstTime = .true.
      Nstory    = 0
      Nstory_MC = 0
      ierr98 = 0
      ierr99 = 0
c      write(*,720) Ifile,Nfiles,file_in
c  1720 format(/' Starting to Analyze data FILE (',I2,'of',I2,'): '/' ',A78)
      CALL open_ntpl_goa(file_in,2) ! second step


      print *,'Second step'

      DO WHILE (.true.)
	 Nstory =  Nstory + 1
         call HGNT(93,Nstory,ierr99)
         IF(ierr99.ne.0) THEN
           Nstory = Nstory - 1
           GOTO 190
         ENDIF	
         CALL hfnt(ntupl_number)
c+ Writing events passed     
      if (Nstory/20000*20000.eq.Nstory.or.Nstory.eq.100) then
         write(*,*) Nstory,' DATA passed         '
      end if
      
      ENDDO  ! next event 

      
C----------------------------------------------------------
 190   continue
	print *,'FILE completed. ',
     &          ' Events written to nt DATA = ',Nstory

        print *,'Do hrend'
	CALL hrend('ESCA')



        CALL hcdir('//PART',' ')
 	CALL hrout(0,icycle,' ')
	CALL hrend('PART')


 

	STOP
	END

