        logical logic
        integer irun
        character*80 mapfile
        
99      format (a80)

        print *,'Put constants in what Map Manager file ?'
        print *,'default = /group/clas/parms/Maps/TAG_CALIB.map'
	read (*,99)mapfile
        if (mapfile.eq.'') mapfile='/group/clas/parms/Maps/TAG_CALIB.map'
        
        print *,'starting what run ?'
	read *,irun

	print *,'Put Tagger constant from local files into the Map Manager, starting run ',irun
        print *,'Into the file ',mapfile

        call tagcalib_put_map(mapfile,irun,logic)
        if(.not.logic) stop 'ERROR'
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine tagcalib_put_map(mapfile,irun,ok)
c  --------
c	Attempts to read calibration data and fill map manager
c
c
       IMPLICIT NONE

       SAVE

        real posTpeakL,posTpeakR,posEpeak
        integer tagTDCbaseEPeakMM,tagTDCbaseTPeakMM
        common/TagTDCbasePeak/posTpeakL(61),posTpeakR(61),posEpeak(384),tagTDCbaseEPeakMM,
     +  tagTDCbaseTPeakMM

        real ECchWidth,TCchWidthL,TCchWidthR
        integer TagTDCCalTMM,TagTDCCalEMM
        common/TagTDCCal/ECchWidth,TCchWidthL(61),TCchWidthR(61),TagTDCCalTMM,
     +   TagTDCCalEMM

        real TagTCi
        integer TagCivalMM
        common/TagCival/TagTCi(121),TagCivalMM

        integer tac_ped_lt,tac_ped_rt,tac_ped_lb,tac_ped_rb,tac_ped_sum1,tac_ped_sum2,tac_ped_sum3,
     +  pc_ped_ve,pc_ped_mn,pc_ped_lt,pc_ped_rb,pc_ped_lb,pc_ped_rt,
     +  ps_ped,TagDSDCalMM
        real tac_tdc_w,pc_TDC_w,ps_tdc_w,
     +  posTACpeak,posPCpeak,posPSpeak,
     +  tac_sca_lt,tac_sca_rt,tac_sca_lb,tac_sca_rb,tac_sca_sum1,tac_sca_sum2,tac_sca_sum3,
     +  timwk1,timwk2,timwk3,PSthreshold
        common/TagDSDCal/tac_ped_lt,tac_ped_rt,tac_ped_lb,tac_ped_rb,
     +  tac_ped_sum1,tac_ped_sum2,tac_ped_sum3,
     +  tac_sca_lt,tac_sca_rt,tac_sca_lb,tac_sca_rb,
     +  tac_sca_sum1,tac_sca_sum2,tac_sca_sum3,
     +  tac_tdc_w,posTACpeak,
     +  pc_ped_ve,pc_ped_mn,pc_ped_lt,pc_ped_rb,pc_ped_lb,pc_ped_rt,pc_TDC_w,posPCpeak,
     +  ps_ped(8),ps_tdc_w(8),posPSpeak(8),
     +  timwk1,timwk2,timwk3,PSthreshold,
     +  TagDSDCalMM

        real tag2tof
        common/tagtotof/tag2tof

	integer TTranslateTo,TagTTranslateMM,TAGTTranslOK
	common/TagTTransTable/TTranslateTo(122),TagTTranslateMM,TAGTTranslOK



c
c  Local tag defined variables:
c  ----------------------------

        logical ok
	real value
        real dummy_real
        real pc_ped(6),ps_walk(3),tac_ped(7),tac_sca(7)
        integer i,dummy_int
	character*100 comments
        character*80 mapfile
        integer irun
        character*80 filename

c_end_var
c
c  executable code for routine tagcal_read_file:
c  ------------------------------------
c
c      
          ok=.true.
c          mapfile='/group/clas/parms/Maps/TAG_CALIB.map'      
c           mapfile='TAG_CALIB.map'      
c     
c
c  ----------------------- T Translation Table
c
        print *,'Reading T Translation table in what file ? '
	read *,filename

          open(50,status='old',err=107,file=filename)
          do i=1,122
           read(50,7) dummy_int,TTranslateTo(i)
          enddo
7         format(I4,I4)
          close(50)

	call map_rem_arr(mapfile,'tag_t','transl_to_table',irun)
	call map_put_int(mapfile,'tag_t','transl_to_table',122,TTranslateTo,irun)

      
c
c  ------------------------ E & T TDC constants:
c 
17      print *,'Reading E TDC calibration constants from file ? '
	read *,filename


          open(50,status='old',err=109,file=filename)
          read(50,9) ECchWidth
9         format(f8.2)
          close(50)

        call map_rem_arr(mapfile,'tag_e','slope',irun)
        call map_put_float(mapfile,'tag_e','slope',1,ECchWidth,irun)



19      print *,'Reading T TDC calibration constants from file ? '
	read *,filename


      open(50,status='old',err=101,file=filename)
	  do i=1,61
	  read(50,1) dummy_real,TCchWidthL(i),TCchWidthR(i)
1	  format(f4.0,f8.2,f8.2)
	  enddo
	  close(50)

	call map_rem_arr(mapfile,'tag_t','slope_left',irun)
        call map_put_float(mapfile,'tag_t','slope_left',61,TCchWidthL,irun)

        call map_rem_arr(mapfile,'tag_t','slope_right',irun)
        call map_put_float(mapfile,'tag_t','slope_right',61,TCchWidthR,irun)

	
c
c  ------------------------ Ci constants:
c
11      print *,'Reading Ci calibration constants from file ? '
	read *,filename

      open(50,status='old',err=102,file=filename)
          do i=1,121
          read(50,2) dummy_real,TagTCi(i)
2	  format(f4.0,f8.2)
	  enddo
          close(50)

        call map_rem_arr(mapfile,'tag_t','ci',irun)
        call map_put_float(mapfile,'tag_t','ci',121,TagTCi,irun)
	
c
c  ------------------------ T peaks positions constants:
c 	
12      print *,'Reading T peak position calibration constants from file ? '
	read *,filename

       open(50,status='old',err=103,file=filename)
	   do i=1,61
	   read(50,3) dummy_real,posTpeakL(i),posTpeakR(i)
3	   format(f4.0,f8.2,f8.2)
	   enddo
	   close(50)

        call map_rem_arr(mapfile,'tag_t','dt_left',irun)
        call map_rem_arr(mapfile,'tag_t','dt_right',irun)

        call map_put_float(mapfile,'tag_t','dt_left',61,posTpeakL,irun)
        call map_put_float(mapfile,'tag_t','dt_right',61,posTpeakR,irun)

c
c  ------------------------ E peaks position constants:
c 	
13      print *,'Reading E peak position calibration constants from file ? '
	read *,filename

       open(50,status='old',err=104,file=filename)
	   do i=1,384
	   read(50,4) dummy_real,posEpeak(i)
4	   format(f4.0,f8.2)
	   enddo
	   close(50)

        call map_rem_arr(mapfile,'tag_e','dt',irun)
        call map_put_float(mapfile,'tag_e','dt',384,posEpeak,irun)
	

c
c  ------------------------ DSD calib. constants:
c 	
14     print *,'Reading DSD calibration constants from file ? '
	read *,filename


       open(50,status='old',err=105,file=filename)
5	  format(f8.2,a100)
	
c ---- PC

	read(50,5) value,comments
        pc_ped(1) = value
	read(50,5) value,comments
        pc_ped(2) = value
	read(50,5) value,comments
        pc_ped(3) = value
	read(50,5) value,comments
        pc_ped(4) = value
	read(50,5) value,comments
        pc_ped(5) = value
	read(50,5) value,comments
        pc_ped(6) = value
	
	read(50,5) value,comments
	pc_TDC_w = value
	read(50,5) value,comments
	posPCpeak = value

        call map_rem_arr(mapfile,'pc','ped',irun)
        call map_rem_arr(mapfile,'pc','tdc_w',irun)
        call map_rem_arr(mapfile,'pc','peak',irun)

        call map_put_float(mapfile,'pc','ped',6,pc_ped,irun)
        call map_put_float(mapfile,'pc','tdc_w',1,pc_TDC_w,irun)
        call map_put_float(mapfile,'pc','peak',1,posPCpeak,irun)

		
c ----- PS

        do i= 1,8
        read(50,5) value,comments
        ps_ped(i) = value
        enddo

        do i= 1,8
        read(50,5) value,comments
        ps_tdc_w(i) = value
        enddo

        do i= 1,8
        read(50,5) value,comments
        posPSpeak(i) = value
        enddo

        read(50,5) value,comments
        ps_walk(1) = value
        read(50,5) value,comments
        ps_walk(2) = value
        read(50,5) value,comments
        ps_walk(3) = value
        read(50,5) value,comments
        PSthreshold = value

        call map_rem_arr(mapfile,'ps','ped',irun)
        call map_rem_arr(mapfile,'ps','tdc_w',irun)
        call map_rem_arr(mapfile,'ps','peak',irun)
        call map_rem_arr(mapfile,'ps','Ethresh',irun)
        call map_rem_arr(mapfile,'ps','walk',irun)

        call map_put_float(mapfile,'ps','ped',8,ps_ped,irun)
        call map_put_float(mapfile,'ps','tdc_w',8,ps_tdc_w,irun)
        call map_put_float(mapfile,'ps','peak',8,posPSpeak,irun)
        call map_put_float(mapfile,'ps','Ethresh',1,PSthreshold,irun)
        call map_put_float(mapfile,'ps','walk',3,ps_walk,irun)

c ---- TAC
	
	read(50,5) value,comments
	tac_ped(1) = value
	read(50,5) value,comments
	tac_ped(2) = value
	read(50,5) value,comments
	tac_ped(3) = value
	read(50,5) value,comments
	tac_ped(4) = value
	read(50,5) value,comments
	tac_ped(5) = value
	read(50,5) value,comments
	tac_ped(6) = value
	read(50,5) value,comments
	tac_ped(7) = value
	
	read(50,5) value,comments
	tac_sca(1) = value
	read(50,5) value,comments
	tac_sca(2) = value
	read(50,5) value,comments
	tac_sca(3) = value
	read(50,5) value,comments
	tac_sca(4) = value
	read(50,5) value,comments
	tac_sca(5) = value
	read(50,5) value,comments
	tac_sca(6) = value
	read(50,5) value,comments
	tac_sca(7) = value
	
	read(50,5) value,comments
	tac_TDC_w = value
	read(50,5) value,comments
	posTACpeak = value
	
 	close(50)

        call map_rem_arr(mapfile,'tac','ped',irun)
        call map_rem_arr(mapfile,'tac','tdc_w',irun)
        call map_rem_arr(mapfile,'tac','peak',irun)
        call map_rem_arr(mapfile,'tac','Escale',irun)

        call map_put_float(mapfile,'tac','ped',7,tac_ped,irun)
        call map_put_float(mapfile,'tac','tdc_w',1,tac_tdc_w,irun)
        call map_put_float(mapfile,'tac','peak',1,posTACpeak,irun)
        call map_put_float(mapfile,'tac','Escale',7,tac_sca,irun)


c
c  ------------------------ tag2tof constant:
c 	

15      print *,'Reading tag2tof constant from file ? '
	read *,filename

          open(50,status='old',err=106,file=filename)
          read(50,6) tag2tof
6         format(f8.2)
          close(50)

         call map_rem_arr(mapfile,'tag2tof','value',irun)
         call map_put_float(mapfile,'tag2tof','value',1,tag2tof,irun)
 	
          return
    


101 	write(6,*)'!! No tagger T-counter TDC calibration file  !!'
        ok = .false.
	goto 11
102     write(6,*)'!! No tagger Ci calibration file !!!'
        ok = .false.
	goto 12
103 	write(6,*)'!! No tagger T counter base peak calibration file !!'
        ok = .false.
	goto 13
104     write(6,*)'!! No tagger E counter base peak calibration file !!!'
        ok = .false.
	goto 14
105     write(6,*)'!! No Down stream devide calibration file !!!'
        ok = .false.
	goto 15
106     write(6,*)'!! No tag2tof calibration file !!!'
        ok = .false.
	return
109 	write(6,*)'!! No tagger E-counter TDC calibration file  !!'
        ok = .false.
	goto 19
 107    write(6,*)'!! No tagger T translation file  !!'
        ok = .false.
	goto 17

        END
c
c------------------------------------------------------------------------------
