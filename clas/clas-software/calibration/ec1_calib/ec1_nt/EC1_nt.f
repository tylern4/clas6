c	This program making calibration of the LAC
c       

      program EC1_nt
      implicit NONE
      
	CHARACTER *80 Fname(10), OutFile
	 	
	include "bcs.inc"
	include "EC1nt.inc"
	include "ntpl_com.inc"
	
	integer*4 N_good_event
	integer*4 istatus1,icycle,ibid
	integer*4 i
	integer*4 ec1N(1000000)
        INTEGER*4 pedest(2,2,1088)
        INTEGER*4 nfiles,runno
        REAL*4    tmin,tmax,timehis
        
        external timehis

      data      pedest/4352*0/
      data      N_good_event/0/
    
      CALL init_par(Nfiles,Fname,OutFile,runno)
      
      CALL read_att_len

c      IF (ped_substr) THEN      
c        CALL read_ped(pedest,spar_ped)
c      END IF
 
      CALL read_cal_cons(runno,pedest)
      CALL prnt_ped(runno,pedest)
      CALL book_ntpl(OutFile)
     
      CALL BOS(IW,Nbcs)
        
      DO Ifile=1, Nfiles
      
        ntime = 0
        firstTime = .true.
        firstNum = 0
        nstory   = 0
        CALL hbook1(Ifile*10+3,'Event Rate, Evnt/sec',50,0.,1000.,0.)
        CALL hbook2(1000+Ifile*10+5,'MIPs spot(sect 1)',
     &              100,0.,200.,100,0.,200.,0.)
        CALL hbook2(2000+Ifile*10+5,'MIPs spot(sect 2)',
     &              100,0.,200.,100,0.,200.,0.)
        
	Nstory    =  0

        CALL FPARM(
     >'OPEN UNIT=12 FILE="'//Fname(Ifile)//'" '//
     >'ACTION=READ RECL=32760 STATUS=OLD FORM=BINARY')
       
        Print *,' Starting to analyze data file ',Fname(Ifile)
     
c  read MCIN data from BOS file until eof reached
c  close file and end run upon eof

        EOF = .false.
        
        DO WHILE (.true.)
          
          CALL read_event
          IF(EOF) goto 1000          
         
          CALL EC1bosFill
          
          CALL TRKbosfill
    
          IF (nEC1 .ne. 0) then 
	    tot_ADC_in (1) = 0.
	    tot_ADC_out(1) = 0.
	    tot_ADC_in (2) = 0.
	    tot_ADC_out(2) = 0.
 	    
 	    CALL ped_subst(pedest)
 	    
 	    CALL select2pmt
 	    
            IF (nEC1 .ne. 0) then 
              nstory = nstory +1
 	      if (nstory.GE.1000000) nstory = 1000000
 	      
 	      ec1N(nstory) = nevent

              CALL fill_ntpl(N_good_event)
            
            end IF
              
          end IF

	 CALL bdrop(iw,'E')
	 CALL bgarb(iw)

  	END DO  ! one event  

 20	 continue
        
        tmin = 1.*deltaT(1)
        tmax = 1.*deltaT(Ntime)
       print *, ' Ntime, tmin, tmax, Nstory ',Ntime,tmin,tmax,Nstory
        CALL hbook1(ifile*10+4,'EC1 Events',50,1.*firstNum,
     &              1.*ec1N(Nstory),0.)

        IF(ntime.gt.0) call hbfun1(ifile*10+1,'Nevent(t,sec)',
     &                             100,tmin,tmax,timehis)
 	DO i=1,Nstory
          call hfill (ifile*10+4,1.*ec1N(i),0.,1.)
        END DO
1000  CONTINUE  ! end of file   
      END DO   ! next data file

c        CALL factors(OutFile,pedest) 
        
 	CALL hrout(0,icycle,' ')
	CALL hrend('ESCA')
 
        CALL FWBOS(IW,IBID,'0',iSTATUS1)	
	print *,' end write status ',iSTATUS1
	print *,icycle
        CALL FCLOS()

	print *,'Ok, RUN completed after reading ',Nstory,' events.'
	print *,'Number of events written to the Ntuple:',N_good_event
	STOP
	END
	
        REAL FUNCTION timehis(t)
        real *4 deltaT,deltaN
        real *4 DIVDIF,t
        integer*4 Ntime
        common /EC1time/Ntime,deltaT(200000),deltaN(200000)
        timehis = DIVDIF(deltaN,deltaT,Ntime,t,1)/10.
        return
        end
