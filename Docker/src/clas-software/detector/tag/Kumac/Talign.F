
c	adds up values of pospeakT.dat and tagcalci.dat
c
c
       IMPLICIT NONE

       SAVE

        real posTpeakL,posTpeakR,posEpeak
        integer tagTDCbaseEPeakMM,tagTDCbaseTPeakMM
        common/TagTDCbasePeak/posTpeakL(61),posTpeakR(61),posEpeak(384),
     +  tagTDCbaseEPeakMM,
     +  tagTDCbaseTPeakMM

        real ECchWidth,TCchWidthL,TCchWidthR
        integer TagTDCCalTMM,TagTDCCalEMM
        common/TagTDCCal/ECchWidth,TCchWidthL(61),TCchWidthR(61),
     +   TagTDCCalTMM,
     +   TagTDCCalEMM

        real TagTCi
        integer TagCivalMM
        common/TagCival/TagTCi(121),TagCivalMM

c
c  Local tag defined variables:
c  ----------------------------

        logical ok
	real value,sum,dummy_real
        real pc_ped(6),ps_walk(3),tac_ped(7),tac_sca(7)
        integer i,dummy_int
	character*100 comments
        character*60 mapfile
        integer irun
        character*60 filename

c_end_var
c
c  executable code for routine tagcal_read_file:
c  ------------------------------------
c
c      
        
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

c
c  ------------------------ T peaks positions constants:
c 	
12      print *,'Reading T peak position calibration 
     +           constants from file ? '
	read *,filename

       open(50,status='old',err=103,file=filename)
	   do i=1,61
	   read(50,3) dummy_real,posTpeakL(i),posTpeakR(i)
3	   format(f4.0,f8.2,f8.2)
	   enddo
	   close(50)
c
c------------------------ adding up
c
13      print *,'put results in what file ? (DEFAULT=Talign.dat)'
	read (*,99) filename
99      format (a60)

        if (filename.eq.'') filename='Talign.dat'

        open(50,status='old',err=104,file=filename)
        goto 204
104     open(50,status='new',err=114,file=filename)
        sum=0
204     do i=1,61
           value = (posTpeakL(i)+posTpeakR(i))/2+TagTCi(2*i-1)
           sum=sum+value
           write(50,4) i,posTpeakL(i),posTpeakR(i),
     +                 TagTCi(2*i-1),value,sum/i
        enddo
        close(50)
4       format(i3,f8.2,f8.2,f8.2,f8.2,f8.2)
    
          goto 14

102     write(6,*)'!! No tagger Ci calibration file !!!'
        ok = .false.
	goto 14
103 	write(6,*)'!! No tagger T counter base peak
     +     calibration file !!'
        ok = .false.
        goto 14
114     write(6,*)'!!could not open '//filename//' file !!'
        ok = .false.

14       end
c
c------------------------------------------------------------------------------
