	subroutine tagcal_read_file(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: tagcal_read_file.F,v 1.15 1999/05/27 18:06:42 anciant Exp $
c
c  Documentation for subroutine tagcal_read_file
c
c  Purpose: THIS FILE IS OBSOLETE, it is not called anymore!!!
c  --------
c	Attempts to read calibration data from files in the CLAS_PARMS area
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c  Energy boundaries of T counters         D. Sober          16 June 1997
c
c  Description of T boundaries read file:
c
c  The energies in the table are always expressed as k/E0 =
c  (photon energy)/(incident electron energy).
c
c  There is  ~20% overlap between the T-counters, so the important
c  energy boundaries are those defining the energies for which a given
c  counter is the FIRST one hit, i.e. k_min and k_max1 in the table.
c  k_max is the maximum value of k/E0 for which the electron hits the
c  counter, but the electron corresponding to k=k_max will always (except
c  for T1) hit the previous counter as well.
c
c    k/E0=           electron path
c   .9227 ------------------------------+----->
c                                       |
c   .9349 -----------------------+------|--->
c                                |     .9356
c   .9446 ----------------+------|-->
c                         |     .9455
c   .9534 ----------------+-->
c                         T1     T2     T3
c
c  ====================================================================
c  T-counter energy boundaries for first T-counter hit
c
c  Calculated without multiple scattering or angular distribution
c  E0 =  2.9800      Rays from file = SN10.OUT     5-May-97  13:07:05
c
c     T_ctr     k_min     k_max1    k_max   (k_max1 = max. k for 1st T)
c         1     .9446     .9534     .9534
c
c   Description of E boundaries read file:
c
c
c E counter channel information            D. Sober          16 June 1997
c
c Monte Carlo output at E0 = 2.98 GeV with brems. angular distribution and
c multiple scattering disabled (i.e. giving nominal channel boundaries),
c and with photon energies generated uniformly in steps of .00001 E0.
c
c E-counter channels:
c      channel 1 = E1 .and..not. E2
c      channel 2 = E1*E2
c      channel 3 = E2 .and..not. E1 .and..not. E3
c      channel 4 = E2*E3
c
c For integer n = 1 to 384:  Channel 2n = coincidence E_n ^ E_n+1
c                            Channel 2n-1 = counter E_n anticoincidencec
c
c Table columns:
c Chan       E-counter channel number 1-767, defined as above
c #          number of events in channel
c >2_E       no. of events in channel for which more than 2 E counters hit
c No_T       no. of events in channel for which no T counters hit
c 2_T        no. of events in channel for which 2 or more T counters hit
c #Dbl       no. of events in "Double Channel" 2n = sum of channel 2n + 2n-1
c            (Note: the Double Channel is probably the cleanest way to
c            view data, since it does not alternate in width like the
c            odd-even single channels)
c k_avg      mean value of k/E0
c k_sd%      standard deviation of k/E0 in percent of E0
c k_min      minimum value of k/E0 in channel
c k_max      maximum value of k/E0 in channel
c
c ============================================================================
c E0 =  2.9800      Rays from file = SN10.OUT               5-May-97  13:07:05
c Energy from  .19000 to  .96000, step  .00001, number    .7700E+05
c Angle  from   .0000 to   .0000, step  .00000
c
c Chan     #  >2_E  No_T   2_T    #Dbl    k_avg   k_sd%   k_min   k_max
c    1   156     0     0     0           .95234  .04503  .95157  .95312
c
c
c
c RESULTING ARRAY definitions (stored in tagcal.CMN) :
c  !!!!REMEMBER lower i ID = higher k/E (k = photon energy, E = beam energy)
c    i.e: the max k/E value of bin i is the minimum k/E value for bin i-1 !!
c
c  T-bondary(i) is the max k/E for bin i
c    this is why there is 122 channels because to have the minimum value of k/E of bin 121 we read the
c    maximum value of a virtual bin 122.
c
c  E-boundary(i) is the max  k/E for bin i
c      min value for bin 767 is given by max value of virtual bin 768.
cc
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "tag_general.PAR"
#include "tagcal.CMN"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
c      
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tagcal_read_file.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.15 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/05/27 18:06:42 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tagcal_read_file')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------

	real value
        real dummy_real
        integer i,dummy_int
	character*100 comments
        character*80 fname
        integer Ilun,Iretb

c_end_var
c
c  executable code for routine tagcal_read_file:
c  ------------------------------------
c
c      
      crmess = '  '
      
      ok= .true.

C Reserve a logical unit number:

          Call ResLun(crname,Ilun,Iretb)
         
c
c  ------------------------ E & T TDC constants:
c 	
c          CRMESS=' '
c          CALL RECMES(CRNAME,'W',CRMESS)

        if (TagTDCCalTMM.eq.0) then
          call revinm('CLAS_PARMS','tagTDCCal.dat',fname)          
 	  open(Ilun,status='old',err=101,file=fname)
          CRMESS='Reading tagger T-counter TDC constants from file : '//fname
          CALL RECMES(CRNAME,'I',CRMESS)
	  do i=1,61
	  read(Ilun,1) dummy_real,TCchWidthL(i),TCchWidthR(i)
1	  format(f4.0,f8.2,f8.2)
	  enddo
	  close(Ilun)
          TagTDCCalTMM = 1
        endif

c
c  ------------------------ E TDC constant:
c 	

11       if (TagTDCCalEMM.eq.0) then
          call revinm('CLAS_PARMS','tagTDCCalE.dat',fname)          
 	  open(Ilun,status='old',err=109,file=fname)
          CRMESS='Reading tagger E-counter TDC constant from file'//fname
          CALL RECMES(CRNAME,'I',CRMESS)
	  read(Ilun,9) ECchWidth
9	  format(f8.2)
	  close(Ilun)
          TagTDCCalEMM = 1
         endif
	
	
	
c
c  ------------------------ Ci constants:
c
	
12      if (TagCivalMM.eq.0) then
          call revinm('CLAS_PARMS','tagCalCi.dat',fname)          
          open(Ilun,status='old',err=102,file=fname)
          CRMESS='Reading tagger Ci constants from file '//fname
          CALL RECMES(CRNAME,'I',CRMESS)
          do i=1,121
          read(Ilun,2) dummy_real,TagTCi(i)
2	  format(f4.0,f8.2)
	  enddo
	  close(Ilun)
          TagCivalMM = 1
        endif
	
c
c  ------------------------ T peaks positions constants:
c 	

13      if (tagTDCbaseTPeakMM.eq.0) then
           call revinm('CLAS_PARMS','tagposTpeak.dat',fname)          
	   open(Ilun,status='old',err=103,file=fname)
           CRMESS='Reading tagger T-counter peak positions from file '//fname
           CALL RECMES(CRNAME,'I',CRMESS)
	   do i=1,61
	   read(Ilun,3) dummy_real,posTpeakL(i),posTpeakR(i)
3	   format(f4.0,f8.2,f8.2)
	   enddo
	   close(Ilun)
           tagTDCbaseTPeakMM = 1
        endif

c
c  ------------------------ E peaks position constants:
c 	
14      if (tagTDCbaseEPeakMM.eq.0) then
           call revinm('CLAS_PARMS','tagposEpeak.dat',fname)          
 	   open(Ilun,status='old',err=104,file=fname)
           CRMESS='Reading tagger E-counter peak positions from file '//fname
           CALL RECMES(CRNAME,'I',CRMESS)
	   do i=1,384
	   read(Ilun,4) dummy_real,posEpeak(i)
4	   format(f4.0,f8.2)
	   enddo
	   close(Ilun)
           tagTDCbaseEPeakMM = 1
         endif
	

c
c  ------------------------ DSD calib. constants:
c 	

15      if (TagDSDCalMM.eq.0) then
          call revinm('CLAS_PARMS','tagdsdcal.dat',fname)          
 	  open(Ilun,status='old',err=105,file=fname)
          CRMESS='Reading DSD calibration constants from file '//fname
          CALL RECMES(CRNAME,'I',CRMESS)

5	  format(f8.2,a100)
	
c ---- PC

	read(Ilun,5) value,comments
	pc_ped_ve = value
	read(Ilun,5) value,comments
	pc_ped_mn = value
	read(Ilun,5) value,comments
	pc_ped_lt = value
	read(Ilun,5) value,comments
	pc_ped_rb = value
	read(Ilun,5) value,comments
	pc_ped_lb = value
	read(Ilun,5) value,comments
	pc_ped_rt = value
	
	read(Ilun,5) value,comments
	pc_TDC_w = value
	read(Ilun,5) value,comments
	posPCpeak = value
		
c ----- PS

	do i= 1,8
	read(Ilun,5) value,comments
	ps_ped(i) = value
	enddo
	
	do i= 1,8
	read(Ilun,5) value,comments
	ps_tdc_w(i) = value
	enddo
	
	do i= 1,8
	read(Ilun,5) value,comments
	posPSpeak(i) = value
	enddo

	read(Ilun,5) value,comments
	timwk1 = value
	read(Ilun,5) value,comments
	timwk2 = value
	read(Ilun,5) value,comments
        timwk3 = value
	read(Ilun,5) value,comments
	PSthreshold = value
	

c ---- TAC
	
	read(Ilun,5) value,comments
	tac_ped_lt = value
	read(Ilun,5) value,comments
	tac_ped_rt = value
	read(Ilun,5) value,comments
	tac_ped_lb = value
	read(Ilun,5) value,comments
	tac_ped_rb = value
	read(Ilun,5) value,comments
	tac_ped_sum1 = value
	read(Ilun,5) value,comments
	tac_ped_sum2 = value
	read(Ilun,5) value,comments
	tac_ped_sum3 = value
	
	read(Ilun,5) value,comments
	tac_sca_lt = value
	read(Ilun,5) value,comments
	tac_sca_rt = value
	read(Ilun,5) value,comments
	tac_sca_lb = value
	read(Ilun,5) value,comments
	tac_sca_rb = value
	read(Ilun,5) value,comments
	tac_sca_sum1 = value
	read(Ilun,5) value,comments
	tac_sca_sum2 = value
	read(Ilun,5) value,comments
	tac_sca_sum3 = value
	
	read(Ilun,5) value,comments
	tac_TDC_w = value
	read(Ilun,5) value,comments
	posTACpeak = value
	
	close(Ilun)
        TagDSDCalMM = 1
        endif

c
c  ----- free logical unit number
c

16       Call ResLun(crname,-Ilun,Iretb)
 	
18      RETURN

101 	CRMESS='!! No tagger T-counter TDC calibration file in CLAS_PARMS !!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 11
102     CRMESS='!! No tagger Ci calibration file in CLAS_PARMS!!!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 13
103 	CRMESS='!! No tagger T counter base peak calibration file in CLAS_PARMS!!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 14
104     CRMESS='!! No tagger E counter base peak calibration file in CLAS_PARMS!!!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 15
105     CRMESS='!! No Down stream devide calibration file in CLAS_PARMS!!!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 16
109 	CRMESS='!! No tagger E-counter TDC calibration file in CLAS_PARMS !!'
        CALL RECMES(CRNAME,'W',CRMESS)
	goto 12

        END
c
c------------------------------------------------------------------------------
