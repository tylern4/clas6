        logical logic
        integer irun
        character*80 mapfile
        
99        format (a80)
        
        print *,'Get constants from what Map Manager file ?'
        print *,'default = /group/clas/parms/Maps/TAG_CALIB.map'
	read (*,99)mapfile
        if (mapfile.eq.'') mapfile='/group/clas/parms/Maps/TAG_CALIB.map'

        print *,'Get constants for what run ?'
	read *,irun

	print *,'Get Tagger constant from the Map Manager at run ',irun
	print *,'from Map file: ',mapfile


        call tagcalib_get_map(mapfile,irun,logic)
        if(.not.logic) stop 'ERROR'
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine tagcalib_get_map(mapfile,irun,ok)
c  --------
c	reads map manager and puts constants in text files
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
     +  TagTDCCalEMM

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
        real pc_ped(6),ps_walk(3),tac_ped(7),tac_sca(7)
        integer i,run0
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
c          mapfile='TAG_CALIB.map'

     
99        format (a80)

c
c
c
c  ----------------------- T Translation Table
c
        print *,'Put T Translation table in what file ? (default=tagTTransTable.dat)'
	read (*,99)filename
        if (filename.eq.'') filename='tagTTransTable.dat'

	call map_get_int(mapfile,'tag_t','transl_to_table',122,TTranslateTo,irun,run0)

          open(50,status='old',err=107,file=filename)
          goto 207
107       open(50,status='new',err=17,file=filename)
207       do i=1,122
           write(50,7) i,TTranslateTo(i)
          enddo
7         format(I4,I4)
          close(50)

c
c  ------------------------ E & T TDC constants:
c 
17      print *,'Put E TDC calibration constants in what file ? (default=tagTDCCalE.dat)'
	read (*,99)filename
        if (filename.eq.'') filename='tagTDCCalE.dat'

        call map_get_float(mapfile,'tag_e','slope',1,ECchWidth,irun,run0)

          open(50,status='old',err=109,file=filename)
          goto 209
109       open(50,status='new',err=19,file=filename)
209        write(50,9) ECchWidth
9         format(f8.2)
          close(50)



19      print *,'Put T TDC calibration constants in file ? (default=tagTDCCal.dat)'
	read (*,99) filename
        if (filename.eq.'') filename='tagTDCCal.dat'

        call map_get_float(mapfile,'tag_t','slope_left',61,TCchWidthL,irun,run0)
        call map_get_float(mapfile,'tag_t','slope_right',61,TCchWidthR,irun,run0)

       open(50,status='old',err=101,file=filename)
       goto 201
101   open(50,status='new',err=11,file=filename)
201   do i=1,61
	  write(50,1) float(i),TCchWidthL(i),TCchWidthR(i)
1	  format(f4.0,f8.2,f8.2)
	  enddo
	  close(50)

c
c  ------------------------ Ci constants:
c
11       print *,'Put Ci calibration constants in file ? (default=tagCalCi.dat)'
	read (*,99)filename
        if (filename.eq.'') filename='tagCalCi.dat'

        call map_get_float(mapfile,'tag_t','ci',121,TagTCi,irun,run0)

        open(50,status='old',err=102,file=filename)
        goto 202
102     open(50,status='new',err=12,file=filename)
202     do i=1,121
          write(50,2) float(i),TagTCi(i)
2	  format(f4.0,f8.2)
	  enddo
          close(50)
	
c
c  ------------------------ T peaks positions constants:
c 	
12      print *,'Put T peak position calibration constants in file ? (default=tagposTpeak.dat) '
	read (*,99)filename
        if (filename.eq.'') filename='tagposTpeak.dat'

        call map_get_float(mapfile,'tag_t','dt_left',61,posTpeakL,irun,run0)
        call map_get_float(mapfile,'tag_t','dt_right',61,posTpeakR,irun,run0)

       open(50,status='old',err=103,file=filename)
       goto 203
103    open(50,status='new',err=13,file=filename)
203    do i=1,61
	   write(50,3) float(i),posTpeakL(i),posTpeakR(i)
3	   format(f4.0,f8.2,f8.2)
	   enddo
	   close(50)

c
c  ------------------------ E peaks position constants:
c 	
13      print *,'Put E peak position calibration constants in file ?(default=tagposEpeak.dat) '
	read (*,99)filename
        if (filename.eq.'') filename='tagposEpeak.dat'

        call map_get_float(mapfile,'tag_e','dt',384,posEpeak,irun,run0)
	

       open(50,status='old',err=104,file=filename)
       goto 204
104    open(50,status='new',err=14,file=filename)
204    do i=1,384
	   write(50,4) float(i),posEpeak(i)
4	   format(f4.0,f8.2)
	   enddo
	   close(50)

c
c  ------------------------ DSD calib. constants:
c 	
14     print *,'Put DSD calibration constants in file ? (default=tagdsdcal.dat)'
	read (*,99)filename
        if (filename.eq.'') filename='tagdsdcal.dat'


       open(50,status='old',err=105,file=filename)
       goto 205
105    open(50,status='new',err=15,file=filename)
5	  format(f8.2,a26,i4)
	
c ---- PC
205     call map_get_float(mapfile,'pc','ped',6,pc_ped,irun,run0)
        call map_get_float(mapfile,'pc','tdc_w',1,pc_TDC_w,irun,run0)
        call map_get_float(mapfile,'pc','peak',1,posPCpeak,irun,run0)

        write(50,5) pc_ped(1),'! pc ped veto '
	write(50,5) pc_ped(2),'! pc ped main'
	write(50,5) pc_ped(3),'! pc ped lt'
	write(50,5) pc_ped(4),'! pc ped rb'
	write(50,5) pc_ped(5),'! pc ped lb'
	write(50,5) pc_ped(6),'! pc ped rt'
	
	write(50,5) pc_TDC_w,'! pc tdc channel width'
	write(50,5) posPCpeak,'! pc peak position (ns)'
		
c ----- PS
        call map_get_float(mapfile,'ps','ped',8,ps_ped,irun,run0)
        call map_get_float(mapfile,'ps','tdc_w',8,ps_tdc_w,irun,run0)
        call map_get_float(mapfile,'ps','peak',8,posPSpeak,irun,run0)
        call map_get_float(mapfile,'ps','Ethresh',1,PSthreshold,irun,run0)
        call map_get_float(mapfile,'ps','walk',3,ps_walk,irun,run0)

        do i= 1,8
        write(50,5) ps_ped(i),'! ps ped ',i
        enddo

        do i= 1,8
        write(50,5) ps_tdc_w(i),'! ps tdc channel width ',i
        enddo

        do i= 1,8
        write(50,5) posPSpeak(i),'! ps peak position (ns) ',i
        enddo

        write(50,5) ps_walk(1),'! PS time walk parameter 1'
        write(50,5) ps_walk(2),'! PS time walk parameter 2'
        write(50,5) ps_walk(3),'! PS time walk parameter 3'
        write(50,5) PSthreshold,'! PS Energy deposit threshold'

c ---- TAC
        call map_get_float(mapfile,'tac','ped',7,tac_ped,irun,run0)
        call map_get_float(mapfile,'tac','tdc_w',1,tac_tdc_w,irun,run0)
        call map_get_float(mapfile,'tac','peak',1,posTACpeak,irun,run0)
        call map_get_float(mapfile,'tac','Escale',7,tac_sca,irun,run0)
	
	write(50,5) tac_ped(1),'! TAC ped lt'
	write(50,5) tac_ped(2),'! TAC ped rt'
	write(50,5) tac_ped(3),'! TAC ped lb'
	write(50,5) tac_ped(4),'! TAC ped rb'
	write(50,5) tac_ped(5),'! TAC ped sum1'
	write(50,5) tac_ped(6),'! TAC ped sum2'
	write(50,5) tac_ped(7),'! TAC ped sum3'
	
	write(50,5) tac_sca(1),'! TAC energy scale factor lt'
	write(50,5) tac_sca(2),'! TAC energy scale factor rt'
	write(50,5) tac_sca(3),'! TAC energy scale factor lb'
	write(50,5) tac_sca(4),'! TAC energy scale factor rb'
	write(50,5) tac_sca(5),'! TAC energy scale factor sum1'
	write(50,5) tac_sca(6),'! TAC energy scale factor sum2'
	write(50,5) tac_sca(7),'! TAC energy scale factor sum3'
	
	write(50,5) tac_TDC_w,'! TAC channel width'
	write(50,5) posTACpeak,'! TACpeak position (ns)'
	
 	close(50)

c
c  ------------------------ tag2tof constant:
c 	
 15     print *,'Put tag2tof constant in file ?(default =tag2tof.dat) '
	read (*,99)filename
        if (filename.eq.'') filename='tag2tof.dat'

        call map_get_float(mapfile,'tag2tof','value',1,tag2tof,irun,run0)
	

       open(50,status='old',err=106,file=filename)
       goto 206
106    open(50,status='new',err=16,file=filename)
206    write(50,6) tag2tof
6      format(f8.2)
	   close(50)

16        return
         END
c
c------------------------------------------------------------------------------
