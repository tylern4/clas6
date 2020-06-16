	subroutine tag_raw_2_BOS(OK,pattern)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_raw_2_BOS.F,v 1.11 1999/03/09 18:48:49 boiarino Exp $
c
c  Documentation for subroutine
c
c  Purpose:    Takes the Raw Banks RC11, RC12 & RC13 and convert them to 
c  --------
c		BOS bank format TAGT & TAGE & DSPC & DSPS & DSTC & CALL & ST0
c		  This should be used only for 97 runs (now early run 98)
c
c  Input Parameters: pattern (integer) : bit pattern of data to be converted to BOS
c  ----------------
c
c	1  : converts raw to TAGT
c   2  : converts raw to TAGE
c	4  : converts raw to DSD banks: DSPC (pair counter), DSTC (total absorption counter), DSPS (pair spectrometer)
c	8  : converts raw to CALL (RF)
c	16 : converts raw to ST0 (start counter)
c
c & any combinations :
c 	3 = TAGT & TAGE
c   5 = TAGT & DSD                     
c   9 = TAGT & CALL
c  17= TAGT & ST0
c   6 = TAGE & DSD
c  10 = TAGE & CALL
c  18 = TAGE & ST0
c  12 = DSD & CALL
c  20 = DSD & ST0
c  24 = CALL & ST0
c  7  = TAGT & TAGE & DSD
c  etc..
c
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------    OK     logical  true = everything went right
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:          Created:  Sat jun 11 14:07:15 EST 1997
c  -------
c
c  Major revisions:
c  ----------------    
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
* 
c BOS common block  uncomment the next line for BOS include file
*      
#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagtcl.CMN"
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok    ! conversion worked or not
      integer pattern
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: tag_raw_2_BOS.F,v 1.11 1999/03/09 18:48:49 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_raw_2_BOS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c        
c_end_var
c
c
c  executable code for routine tag_raw_2_BOS:
c----6----------------------------------------------------------------72
*       
        integer NWRITE

        logical needtagt,needtage,needdsd,needrf,needst
 
	integer mamind,mbank,ndrop
	external mamind,mbank,ndrop

	integer nami,ind,i,j,k
		
	integer Raw_Data(2000)
	
	integer channel,Slot_Id,value
	integer*2 channel2, value2
	integer Nb_Left,Nb_Right,Nb_Hits,Nb_Word,Nb_RF_hits
	integer tc_left_id(61),tc_right_id(61),tc_left_tdc(61),tc_right_tdc(61)
	integer Tag_T_Id(61),Tag_T_Left_TDC(61),Tag_T_Right_TDC(61)
	integer Tag_E_Id(MAXEC),Tag_E_TDC(MAXEC)

	integer St_Id(6),St_TDC(6)
	
	integer rawrf1,rawrf2
	
        integer rawtac_adc_lt,rawtac_adc_rt,rawtac_adc_lb,rawtac_adc_rb,rawtac_adc_sum,rawtac_tdc
        integer rawpc_adc_ve,rawpc_adc_mn,rawpc_adc_lt,rawpc_adc_rb,rawpc_adc_lb,rawpc_adc_rt,rawpc_tdc
	integer rawps_adc_l(4),rawps_adc_r(4),rawps_tdc_l(4),rawps_tdc_r(4),nb_pshits
     
	integer last_slot
c
       
c
c -----------	initialise condition variable
c
        needtagt = (iand(pattern,B'0001').ne.0)
        needtage = (iand(pattern,B'0010').ne.0)
        needdsd = (iand(pattern,B'0100').ne.0)
        needrf = (iand(pattern,B'1000').ne.0)
        needst = (iand(pattern,B'10000').ne.0)
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         write(crmess,1) pattern
1        format ('processing of the first raw to BOS conversion with value: ',I2)
         CALL RECMES(CRNAME,'I',CRMESS)
         if (needtagt) CALL RECMES(CRNAME,'I','Generation of TAGT from RAW RC12')
         if (needtage) CALL RECMES(CRNAME,'I','Generation of TAGE from RAW RC17')
         if (needdsd) CALL RECMES(CRNAME,'I','Generation of DSPC,DSPS,DSTC from RAW RC12')
         if (needrf) CALL RECMES(CRNAME,'I','Generation of CALL (RF) from RAW RC13')
         if (needst) CALL RECMES(CRNAME,'I','Generation of ST0 from RAW RC13')
       ENDIF	
c

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	converts RC12 to T-counters & DSD BOS bank      c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (needtagt.or.needdsd) then ! do we need RC12 ?

c
c     Load raw data's coded on 4 bytes format. ROC 12 (T counters & DSD)
c

       call BLIST(iw,'E+','RC12')
       nami = MAMIND(iw,'RC12')
       ind = IW(nami)

       OK = ind.ne.0
       if (OK) then
c
c	Copying raw data to temporary array so that data can be reformatted locally
c
          Nb_Word = iw(ind)
          do i=1,Nb_Word
            Raw_Data(i) =  iw(ind+i)
          enddo
       else
	 CRMESS = 'no data from ROC 12 available'
	 call RECMES(CRNAME,'W',CRMESS)
	 return
       endif
       
c
c  ------ Initializing
c
	Nb_Left = 0
	Nb_Right = 0
        rawtac_tdc = 0
        rawpc_tdc = 0
        do i=1,4
	rawps_tdc_l(i) = 0
	rawps_tdc_r(i) = 0
	enddo
	
c
c  ------ reading
c
	

       do i=1,Nb_Word

         Slot_Id=ishft(Raw_Data(i),-27)

c --- by default we consider the module to be a TDC, if not we will change
c --- channel and value
         channel=ISHFT(IAND(Raw_Data(i),'007F0000'x),-16)+1
         value=IAND(Raw_Data(i),'00000FFF'x) !TDC 12 bit
       
         if (needdsd.and.Slot_Id.eq.DSD_ADC_SLOT) then
c
c     Downstream counters ADC, 
c
             channel=ISHFT(IAND(Raw_Data(i),'007E0000'x),-17)
             value=IAND(Raw_Data(i),'00003FFF'x) !ADC 14 bits

c ----- TAC
             if (channel.eq.32) then
		rawtac_adc_lt = value
             else if (channel.eq.33) then
		rawtac_adc_rt = value
             else if (channel.eq.34) then
		rawtac_adc_lb = value
             else if (channel.eq.35) then
		rawtac_adc_rb = value
             else if (channel.eq.36) then
		rawtac_adc_sum = value
c -----PC            
             else if (channel.eq.37) then
		rawpc_adc_mn = value 
             else if (channel.eq.38) then
		rawpc_adc_lt = value 
             else if (channel.eq.39) then
		rawpc_adc_rb = value 
             else if (channel.eq.40) then
		rawpc_adc_rt = value 
             else if (channel.eq.41) then
		rawpc_adc_lb = value 
             else if (channel.eq.50) then
		rawpc_adc_ve = value 
c ----- PS            
             else if (channel.eq.42) then
		rawps_adc_l(1) = value
             else if (channel.eq.43) then
		rawps_adc_l(2) = value 
             else if (channel.eq.44) then
		rawps_adc_l(3) = value 
             else if (channel.eq.45) then
		rawps_adc_l(4) = value 
             else if (channel.eq.46) then
		rawps_adc_r(1) = value 
             else if (channel.eq.47) then
		rawps_adc_r(2) = value 
             else if (channel.eq.48) then
		rawps_adc_r(3) = value 
             else if (channel.eq.49) then
		rawps_adc_r(4) = value
	     endif
             
         else if (needdsd.and.Slot_Id.eq.DSD_TDC_SLOT) then
c
c     Downstream counters TDC
c
c ----- TAC
             if (channel.eq.22) then
		rawtac_tdc = value
c ----- PC
             else if (channel.eq.23) then
		rawpc_tdc = value
c ----- PS
             else if (channel.eq.24) then
		rawps_tdc_l(1) = value
             else if (channel.eq.25) then
		rawps_tdc_l(2) = value 
             else if (channel.eq.26) then
		rawps_tdc_l(3) = value 
             else if (channel.eq.27) then
		rawps_tdc_l(4) = value 
             else if (channel.eq.28) then
		rawps_tdc_r(1) = value 
             else if (channel.eq.29) then
		rawps_tdc_r(2) = value 
             else if (channel.eq.30) then
		rawps_tdc_r(3) = value 
             else if (channel.eq.31) then
		rawps_tdc_r(4) = value    
             endif

         else if (needtagt.and.Slot_Id.eq.TC_LEFT_SLOT) then
c
c        Left T-counters 
c
             if (value.gt.0.and.value.lt.4093.) then
		Nb_Left = Nb_Left + 1
		tc_left_id(Nb_Left) = channel
		tc_left_tdc(Nb_Left) = value
	     endif
		
         else if (needtagt.and.Slot_Id.eq.TC_RIGHT_SLOT) then
c
c	right T-counters
c
             if(value.gt.0.and.value.lt.4093.) then
               Nb_Right = Nb_Right + 1
               tc_right_id(Nb_Right) = channel
               tc_right_tdc(Nb_Right) = value
              endif 
                
         endif
       enddo
       
c----------------------------------------------------
c
c	NOW fill the TAGT BOS BANK after some processing.
c
	if (needtagt) then
c	
c ------ first we fill the array Tag_T_Id, Tag_T_Left_TDC, Tag_T_Right_TDC 
c
	k = 0

	do i=1,Nb_Left
	  do j=1,Nb_Right
		if(tc_left_id(i) .eq. tc_right_id(j)) then
		  k = k + 1
		  Tag_T_Id(k)=tc_right_id(j)
		  Tag_T_Left_TDC(k)=tc_left_tdc(i)
		  Tag_T_Right_TDC(k)=tc_right_tdc(j)
c
c		  disable these counters to avoid double counting. Will be left, counters with no
c		  opposite matching tdc
c
		  tc_right_id((j)) = -1
		  tc_left_id(i) = -2
		endif
	  enddo
	enddo
	

c ------ Then we keep on filling Tag_T_Id, Tag_T_Left_TDC, Tag_T_Right_TDC 
c ------ with the values for single hit only...

	do i=1,Nb_Left
	  if(tc_left_id(i) .gt. 0) then
		k = k + 1
		Tag_T_Id(k)=tc_left_id(i)
		Tag_T_Left_TDC(k)=tc_left_tdc(i)
		Tag_T_Right_TDC(k)=0
c
c		disable this counter to avoid double counting
c
		tc_left_id(i) = -2
	  endif
	enddo

	do i=1,Nb_Right
	  if(tc_right_id(i) .gt. 0) then
		k = k + 1
		Tag_T_Id(k)=tc_right_id(i)
		Tag_T_Left_TDC(k)=0
		Tag_T_Right_TDC(k)=tc_right_tdc(i)
c
c		  disable this counter to avoid double counting
c
		tc_right_id(i) = -1
	  endif
	enddo	

	Nb_Hits = k
	if (Nb_Hits.lt.61) Tag_T_Id(Nb_Hits+1) = 0
	
c
c	Now that all the hits are grouped  by L+R, we put them in a TAGT bos bank
c	WARNING : Raw BOS bank TAGT is B16 (2 bytes)
c
	ind = MBANK(iw,TagTData_BOS_name,1,TagTData_BOS_size,Nb_Hits)

	do i=1, Nb_Hits
	  j=(i-1)*TagTData_BOS_size
	  channel2 = Tag_T_Id(i)
	  IW16(2*ind+j+1) = channel2
	  value2 = Tag_T_Left_TDC(i)
	  IW16(2*ind+j+2) = value2
	  value2 = Tag_T_Right_TDC(i)
	  IW16(2*ind+j+3) = value2
	enddo

	endif ! needtagt

c
c
c-------------- filling DSD BOS BANKs

       if (needdsd) then

c----------------------------------------------------
c	Filling DSPC BOS bank
c	WARNING : Raw BOS bank DSPC is B16 (2 bytes)
c
	if (rawpc_tdc.ne.0) then
	  ind = MBANK(iw,'DSPC',1,8,1)

	  IW16(2*ind + 1) = 1
	  IW16(2*ind + 2) = rawpc_tdc
	  IW16(2*ind + 3) = rawpc_adc_mn
	  IW16(2*ind + 4) = rawpc_adc_lt
	  IW16(2*ind + 5) = rawpc_adc_rb
	  IW16(2*ind + 6) = rawpc_adc_lb
	  IW16(2*ind + 7) = rawpc_adc_rt
	  IW16(2*ind + 8) = rawpc_adc_ve
	else	  
	  ind = MBANK(iw,'DSPC',1,8,0)
	endif
	
c----------------------------------------------------
c	Filling DSPS BOS bank
c	WARNING : Raw BOS bank DSPS is B16 (2 bytes)
c
	nb_pshits = 0
	do i=1,4
	   if (rawps_tdc_l(i).ne.0) nb_pshits = nb_pshits + 1
           if (rawps_tdc_r(i).ne.0) nb_pshits = nb_pshits + 1
	enddo
	
	ind = MBANK(iw,'DSPS',1,3,nb_pshits)
	 
	nb_pshits = 0
	do i=1,4
	   if (rawps_tdc_l(i).ne.0) then
             nb_pshits = nb_pshits + 1
	     j=(nb_pshits-1)*3
	     IW16(2*ind + j + 1) = i
	     IW16(2*ind + j + 2) = rawps_tdc_l(i)
	     IW16(2*ind + j + 3) = rawps_adc_l(i)
           endif

           if (rawps_tdc_r(i).ne.0) then
             nb_pshits = nb_pshits + 1
	     j=(nb_pshits-1)*3
	     IW16(2*ind + j + 1) = i + 4
	     IW16(2*ind + j + 2) = rawps_tdc_r(i)
	     IW16(2*ind + j + 3) = rawps_adc_r(i)
           endif
	enddo
	
c----------------------------------------------------
c	Filling DSTC BOS bank
c	WARNING : Raw BOS bank DSTC is B16 (2 bytes)
c
	if (rawtac_tdc.ne.0) then
	  ind = MBANK(iw,'DSTC',1,9,1)
	  
	  IW16(2*ind + 1) = 1
	  IW16(2*ind + 2) = rawtac_tdc
	  IW16(2*ind + 3) = rawtac_adc_lt
	  IW16(2*ind + 4) = rawtac_adc_rt
	  IW16(2*ind + 5) = rawtac_adc_lb
	  IW16(2*ind + 6) = rawtac_adc_rb
	  IW16(2*ind + 7) = rawtac_adc_sum
	  IW16(2*ind + 8) = 0
	  IW16(2*ind + 9) = 0
	else	  
	  ind = MBANK(iw,'DSTC',1,9,0)
	endif

	endif ! needdsd
	endif ! RC12

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	converts RC17 to E-counters TAGE BOS bank       c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (needtage) then ! do we need tage/RC17

c----------------------------------------------------
c     read raw RC17
c
       call blist(iw,'E+','RC17')
       nami = mamind(iw,'RC17')
       ind = IW(nami)
       
       OK = ind.ne.0
       if (OK) then
c
c	Copying raw data to temporary array so that data can be reformatted locally
c
          Nb_Word = iw(ind)
          do i=1,Nb_Word
            Raw_Data(i) =  iw(ind+i)
          enddo
       else
	 CRMESS = 'no data from ROC 17 available'
	 call RECMES(CRNAME,'W',CRMESS)
	 return
       endif
c
c  ------ Initializing
c
	Nb_Hits = 0
	last_slot = -1  ! for some reason the first value of each slot is dummy
       
c
c  ------  reading
c
	i=1
	do while(i.le.Nb_Word.and.Nb_Hits.lt.MAXEC) 
          Slot_Id=ishft(Raw_Data(i),-27)
c
c  ------------- E counters  -------------------------------------
c

          if (last_slot.eq.Slot_Id.and.Slot_Id.ne.31.and.Slot_Id.ne.0) then
             channel=ISHFT(IAND(Raw_Data(i),'00FE0000'x),-17)
             value=IAND(Raw_Data(i),'0000FFFF'x) !TDC 16 bit
                    Nb_Hits=Nb_Hits+1
                    Tag_E_TDC(Nb_Hits) = value
                    if (Slot_Id.eq.6) then
                    	Tag_E_Id(Nb_Hits) = channel + 1
                    else if (Slot_Id.eq.10) then
                    	Tag_E_Id(Nb_Hits) = channel + 97
                    else if (Slot_Id.eq.14) then
		    	Tag_E_Id(Nb_Hits) = channel + 193
                    else if (Slot_Id.eq.18) then
                    	Tag_E_Id(Nb_Hits) = channel + 289
                    else
                        write(crmess,2) Slot_Id
2                       format ('Error in E-counter conversion from raw, bad slot : ',I2)
                        CALL RECMES(CRNAME,'I',CRMESS)
                        Tag_E_Id(Nb_Hits) = -1
                    endif
          else if (Slot_Id.ne.31) then
                last_slot = Slot_Id
          endif
        i = i + 1
        enddo

       if (Nb_Hits.lt.MAXEC) Tag_E_Id(Nb_Hits + 1 ) = 0 ! this means this is the end of the E table.
 		
c	Now that all the hits are grouped we put them in a raw bos bank
c	WARNING : Raw BOS bank TAGE is B16 (2 bytes)
c
	ind = MBANK(iw,TagEData_BOS_name,1,TagEData_BOS_size,Nb_Hits)

	do i=1, Nb_Hits
	  j=(i-1)*TagEData_BOS_size
	  channel2 = Tag_E_Id(i)
	  IW16(2*ind+j+1) = channel2
	  value2 = Tag_E_TDC(i)
	  IW16(2*ind+j+2) = value2
	enddo

        endif ! needtage/RC17
	
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	converts RC13 to RF & ST         BOS bank       c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if (needrf.or.needst) then ! do we need to read RC13?

c----------------------------------------------------
c
c	now deal with RC13.
c
c
       call blist(iw,'E+','RC13')
       nami = mamind(iw,'RC13')
       ind = IW(nami)

       OK = ind.ne.0
       if (OK) then
c
c	Copying raw data to temporary array so that data can be reformatted locally
c
          Nb_Word = iw(ind)
          do i=1,Nb_Word
            Raw_Data(i) =  iw(ind+i)
          enddo
       else
	 CRMESS = 'no data from ROC 13 available'
	 call RECMES(CRNAME,'W',CRMESS)
	 return
       endif

	nb_RF_hits = 0
	Nb_Hits = 0
	j = 0
	do i=1,6
	  St_TDC(i) = 0
	enddo

       do i=1,Nb_Word
          Slot_Id=ishft(Raw_Data(i),-27)
          channel=ISHFT(IAND(Raw_Data(i),'007F0000'x),-16)
          value=IAND(Raw_Data(i),'00000FFF'x) !TDC 12 bit
          
          if (needst.and.Slot_Id.eq.8) then
c
c   ------   start counter
c
          	if (channel.eq.32) then
		  j = j + 1
		  St_Id(j) = 1
		  St_TDC(1) = value
		  Nb_Hits = Nb_Hits +1
          	else if (channel.eq.33) then
		  j = j + 1
		  St_Id(j) = 2
		  St_TDC(2) = value
		  Nb_Hits = Nb_Hits +1
          	else if (channel.eq.34) then
		  j = j + 1
		  St_Id(j) = 3
		  St_TDC(3) = value
		  Nb_Hits = Nb_Hits +1
          	else if (channel.eq.35) then
		  j = j + 1
		  St_Id(j) = 4
		  St_TDC(4) = value
		  Nb_Hits = Nb_Hits +1
          	else if (channel.eq.36) then
		  j = j + 1
		  St_Id(j) = 5
		  St_TDC(5) = value
		  Nb_Hits = Nb_Hits +1
          	else if (channel.eq.37) then
		  j = j + 1
		  St_Id(j) = 6
		  St_TDC(6) = value
		  Nb_Hits = Nb_Hits +1
                endif

          else if (needrf.and.Slot_Id.eq.13) then
c
c -------     RF signal
c
         	if (channel.eq.48) then
                  nb_RF_hits = nb_RF_hits + 1
                  rawrf1 = value
         	else if (channel.eq.49) then
                  nb_RF_hits = nb_RF_hits +1
                  rawrf2 = value
           	endif
          endif 
       enddo	
   
c----------------------------------------------------
c	Filling CALL BOS bank
c	WARNING : Raw BOS bank CALL is B16 (2 bytes)
c
        if (needrf) then

	ind = MBANK(iw,'CALL',0,3,nb_RF_hits)

        j = -3
        if (rawrf1.ne.0) then
          j = j + 3
	  channel2 = 6
	  IW16(2*ind + j + 1) = channel2
	  value2 = rawrf1
	  IW16(2*ind + j + 2) = value2
	  value2 = 0
	  IW16(2*ind + j + 3) = value2
        endif
        if (rawrf2.ne.0) then
          j = j + 3
          channel2 = 7
	  IW16(2*ind + j + 1) = channel2
	  value2 = rawrf2
	  IW16(2*ind + j + 2) = value2
	  value2 = 0
	  IW16(2*ind + j + 3) = value2
        endif
        
        endif ! needrf
c----------------------------------------------------
c	Filling ST0 BOS bank
c	WARNING : Raw BOS bank ST0 is B16 (2 bytes)
c
        if (needst) then
	ind = MBANK(iw,'ST0 ',1,3,Nb_Hits)

	do i=1, Nb_Hits
	  j=(i-1)*3
	  channel2 = St_Id(i)
	  IW16(2*ind+j+1) = channel2
	  value2 = St_TDC(St_Id(i))
	  IW16(2*ind+j+2) = value2
	  value2 = 0
	  IW16(2*ind+j+3) = value2
	enddo

	endif ! needst

       endif ! RC13

       return
       end
