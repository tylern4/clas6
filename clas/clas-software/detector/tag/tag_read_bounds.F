	subroutine tag_read_bounds(ok,where,energy)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_bounds.F,v 1.5 2008/04/30 18:19:59 pasyuk Exp $X%
c
c  Documentation for subroutine tag_read_bounds
c
c  Purpose: 
c  --------
c	Attempts to read boundaries energies from files in CLAS_PARMS area
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  where   (int): ( 1 = local, otherwise CLAS_PARMS)
c  ----------------   energy (real): beam energy in Gev
c
c  Output Parameters:  ok (boolean) things worked fine
c  -----------------
c
c  Called from: tagcal_read_file AND tagcal_read_local_file
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes: Modification of may 1999
c  ------
c  Dan Sober Notes:
c
c
c 1.  INTRODUCTION
c
c The energy calibration of the tagger changes slowly as a function of E0,
c because the magnetic field at the full-energy orbit decreases relative to the
c field at the center of the pole as the magnet approaches saturation.
c The effect is small (always less than 3 fine channels of ~.001 E0) at energies 
c below 4.1 GeV, but increases rapidly at higher energies, as shown in Table 1.
c The effect is largest for low photon energies, where the energy of the
c detected  electron is a large fraction of E0.
c 
c --------------------------------------------------------------------------------
c   Table 1.  Values of k/E0 vs. fine channel for 5 different energies
c             (channel upper limit with no angular smearing).
c             
c    E0 =    0.90 GeV     2.39 GeV        4.14 GeV        5.56 GeV        6.12 GeV
c E channel  
c  100       .85978       .86042          .86058          .86035          .85966
c  200       .76309       .76357          .76361          .76204          .76047
c  300       .66681       .66682          .66664          .66356          .66121
c  400       .57056       .56990          .56941          .56484          .56177
c  500       .47424       .47285          .47209          .46616          ,46244
c  600       .37701       .37523          .37427          .36722          .36312
c  700       .27888       .27710          .27608          .26841          .26404
c --------------------------------------------------------------------------------
c 
c 
c The calculations used in creating these files consist of the following steps:
c (1) For each of the 10 field maps, calculate a file of trajectories by 
c     ray-tracing using the code SNAKE (actually my modification of an old
c     version of SNAKE.)  The present version of the trajectory file consists
c     of 235 rays (47 energies times 5 angles per energy).  I have tested the
c     energy steps and confirmed that interpolation between these energies
c     leads to energy errors less than 5E-5, or 0.05 fine channel.
c (2) Using the 235-ray files, run my tagger Monte Carlo code in two separate
c     modes:
c     (a) uniform energy steps (< 1.E-6 E0), no bremsstrahlung angle, no
c         multiple scattering -- to determine energy boundaries of counters
c         and channels.
c     (b) Monte Carlo energy distribution (as 1/k), bremsstrahlung and
c         multiple-scattering angular distributions enabled -- to determine
c         correlations between counters.
c 
c 
c 
c The data format of the new files, and my suggestions for reading them,
c follow below.
c 
c tagT-boundaries.nnnn:
c 
c    #     Tctr      kmin kminfirst      kmax   ! Last of 6 comment lines
c             1   0.94476   0.95347   0.95347   ! format(i10,3f10.5)
c             2   0.93500   0.94476   0.94570
c           ...
c           
c           (kminfirst is the minimum k for which this T counter is the first
c           one hit.)
c 
c       do i=1,61
c  1000  read(Ilun,'(a)') line                ! read 80-character line
c        if(index(line,'#').ne.0) go to 1000  ! skip lines with '#'       
c        read (line,'(i10,3f10.5)')) itest,Tboundary(2*i+1),dummy_real,
c      1  Tboundary(2*i-2)
c        if(itest.ne.i) go to 5000            ! test for data corruption
c       end do
c 
c 
c tagE-boundaries.nnnn:
c 
c    # Echan    kmin      kmax      kavg     ksdev   ! Last of 6 comment lines
c        1   0.95169   0.95322   0.95246   0.04434   ! format(i5,4f10.5)
c        2   0.95097   0.95169   0.95133   0.02080
c        ...
c 
c       do i=1,767         
c 1000   read(iunit,'(a)') line               ! read 80-character line
c         if(index(line,'#').ne.0) go to 1000  ! skip lines with '#'
c         read(line,'(i5,4f10.5)') itest,Eboundary(i+1),dummy,Eaverage(i),
c      1   Edeviation(i)
c         if(itest.ne.i) go to 5000            ! test for data corruption
c       end do
c 
c 
c tagETcoinc.nnnn:
c    # Ectr Tmin Tmax T1min T1max TCHANmin TCHANmax  ! Last of 6 comment lines
c        1    1    1    1    1    1    1             ! format(7i5)
c        2    1    1    1    1    1    1
c      ...
c 
c       (T1min and T1max are the first and last T's for which that T is the
c       first one in coincidence with the E.  TCHANmin and TCHANmax refer to
c       the T-channels 1-121.)
c 
c       do i=1,384
c  1000  read(Ilun,'(a)') line                  ! read 80-character line
c        if(index(line,'#').ne.0) go to 1000    ! skip lines with '#'
c        read(line,'(7i5)') itest,tmin(i),tmax(i)
c        if(itest.ne.i) go to 5000              ! test for data corruption
c        tbinmin(tag_bin(i,1)) = tag_bin(tmin(i),1)
c        if(i.lt.384) tbinmin(tag_bin(i,2)) = tag_bin(tmin(i),1)
c       end do
c 
c 

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
c  Author:   Dan Sober & Eric Anciant      Created:  May 1999
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
#include "tagtcl.CMN"
#include "tagRunControl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
      integer where
      real energy
c      
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_read_bounds.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.5 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2008/04/30 18:19:59 $')
      PARAMETER (CAUTHO= '$Author: pasyuk $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_bounds')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  function type :
c  --------------
	integer tag_bin

c
c  Local tag defined variables:
c  ----------------------------
c	real value
        real dummy_real
        integer i,itest
c	character*100 comments
        character*80 fname,line
        integer Ilun,Iretb
c	real TBoundary(122)
c	real EBoundary(768),Eaverage(768),Edeviation(768)
c        integer tbin(384),tmax(384),tbinmin(767)

        logical endfile,FoundEnergy,Corrupted
        real BestEnergy,NewEnergy
        character*7 dummarray
	integer irun
	integer run0
	common/tagrun/irun

c_end_var
c
c  executable code for routine tagcal_read_file:
c  ------------------------------------
c
c      
	crmess = '  '
      
C Reserve A Logical Unit Number:
	Call ResLun(Crname,Ilun,Iretb)
	
	ok= .true.
	
 16	if (tagTboundsMM.eq.0) then
	   if (where.eq.1) then
	      open (unit=Ilun,status='old',err=116,file='tagT-boundaries_ALL.dat')
	      CRMESS='Reading energy T boundaries from local file: tagT-boundaries_ALL.dat'
	   else
	      call revinm('CLAS_PARMS','tagT-boundaries_ALL.dat',fname)          
	      open (unit=Ilun,status='old',err=106,file=fname)
	      CRMESS='Reading energy T boundaries from file '//fname
	   endif
	   CALL RECMES(CRNAME,'I',CRMESS)
	   endfile = .FALSE.
	   BestEnergy = 0.
	   do while(.not.endfile)
c       ---------- find next energy
	      FoundEnergy = .FALSE.
	      Corrupted = .FALSE.
	      do while(.not.FoundEnergy)
		 read(Ilun,'(a)',END=506) line
		 if (index(line,'E0').ne.0) then
		    FoundEnergy = .TRUE.
		    read (line,'(a7,f6.4)') dummarray,NewEnergy
		 endif
	      enddo
	      if (abs(newenergy-energy).lt.abs(bestenergy-energy)) then ! most trivial algorithm: take closer value
		 bestenergy = newenergy
c------------read values.
		 Corrupted = .TRUE. ! if loop ends before the end, detect corupted files
 1016		 read(Ilun,'(a)',END=506) line ! read 80-character line
		 if(index(line,'#').ne.0) go to 1016 ! skip lines with '#'       
		 read (line,'(i10,3f10.5)') itest,Tboundary(3),dummy_real,Tboundary(1)
		 do i=2,60
 1006		    read(Ilun,'(a)',END=506) line ! read 80-character line
		    if(index(line,'#').ne.0) go to 1006	! skip lines with '#'       
		    read (line,'(i10,3f10.5)') itest,Tboundary(2*i+1),dummy_real,
     +                    Tboundary(2*i-2)
		    if(itest.ne.i) go to 506 ! test for data corruption
		 end do
		 read(Ilun,'(a)',END=506) line ! read 80-character line
		 read (line,'(i10,3f10.5)') itest,Tboundary(122),dummy_real,Tboundary(120)
	      endif		! if (abs().lt.abs())
	      goto 206
c--------------handle end of file
 506	      endfile=.TRUE.    
	      if (corrupted) then
		 CRMESS='T-counter boundaries file corrupted !'
		 CALL RECMES(CRNAME,'W',CRMESS)
		 ok=.FALSE.
	      endif
	      if (BestEnergy.eq.0) then
		 CRMESS='Could not find energy values for T boundaries'
		 CALL RECMES(CRNAME,'W',CRMESS)
		 ok=.FALSE.
	      else
		 write(CRMESS,*) ' T boundaries found for energy ',BestEnergy,' Gev'
		 call RECMES(CRNAME,'I',CRMESS)
		 tagTboundsMM = 1
	      endif 
 206	   enddo		! do while(.not.endfile)
	   close(Ilun)
        endif
	
 17	if (tagEboundsMM.eq.0) then
	   if (where.eq.1) then
	      open (unit=Ilun,status='old',err=117,file='tagE-boundaries_ALL.dat')
	      CRMESS='Reading energy E boundaries from local file: tagE-boundaries_ALL.dat'
	   else
	      call revinm('CLAS_PARMS','tagE-boundaries_ALL.dat',fname)          
	      open (unit=Ilun,status='old',err=107,file=fname)
	      CRMESS='Reading energy E boundaries from file '//fname
	   endif
	   CALL RECMES(CRNAME,'I',CRMESS)
	   
	   endfile = .FALSE.
	   BestEnergy = 0.
	   do while(.not.endfile)
c       ---------- find next energy
	      FoundEnergy = .FALSE.
	      Corrupted = .FALSE.
	      do while(.not.FoundEnergy)
		 read(Ilun,'(a)',END=507) line
		 if (index(line,'E0').ne.0) then
		    FoundEnergy = .TRUE.
		    read (line,'(a7,f6.4)') dummarray,NewEnergy 
		 endif
	      enddo
	      if (abs(newenergy-energy).lt.abs(bestenergy-energy)) then ! most trivial algorithm: take closer value
		 bestenergy = newenergy
c------------read values.
		 Corrupted = .TRUE. ! if loop ends before the end, detect corupted files
 1017		 read(Ilun,'(a)',END=507) line ! read 80-character line
		 if(index(line,'#').ne.0) go to 1017 ! skip lines with '#'
		 read(line,'(i5,4f10.5)') itest,Eboundary(2),Eboundary(1),Eaverage(1),Edeviation(1)
		 do i=2,767         
 1007		    read(Ilun,'(a)',END=507) line ! read 80-character line
		    if(index(line,'#').ne.0) go to 1007	! skip lines with '#'
		    read(line,'(i5,4f10.5)') itest,Eboundary(i+1),dummy_real,Eaverage(i),
     +                   Edeviation(i)
		    if(itest.ne.i) go to 507 ! test for data corruption
		 end do
	      endif		! if (abs().lt.abs())
	      goto 207
c--------------handle end of file
 507	      endfile=.TRUE.    
	      if (corrupted) then
		 CRMESS='E-counter boundaries file corrupted !'
		 CALL RECMES(CRNAME,'W',CRMESS)
		 ok=.FALSE.
	      endif 
	      if (BestEnergy.eq.0) then
		 CRMESS='Could not find energy values for E boudnaries'
		 CALL RECMES(CRNAME,'W',CRMESS)
		 ok=.FALSE.
	      else
		 write(CRMESS,*) ' E boundaries found for energy ',BestEnergy,' Gev'
		 call RECMES(CRNAME,'I',CRMESS) 
		 tagEboundsMM = 1
	      endif
 207	   enddo		! do while(.not.endfile)
	   close(Ilun)
        endif
	goto 11
 107	CRMESS='No E-counter boundaries file in CLAS_PARMS'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
	goto 11
 117	CRMESS='No local E-counter boundaries file'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
        goto 11

c
c  =----- E/T geometric matching map
c
 11	if(use_tgtlr .eq. 1 .and. et_map_from_db .eq. 1) then ! <read from db>
	   call revinm('CLAS_PARMS','Maps/TAG_CALIB.map',fname)
	   call map_get_int(fname, 'tag_e', 'tch_min', 384,
     +  	tchmin, irun, run0)
	   CRMESS='Reading tch_min for ET map from caldb'
	   CALL RECMES(CRNAME,'I',CRMESS)
	   if (run0.lt.0) then
	      CRMESS='Problem while reading tch_min for ET map from caldb'
	      CALL RECMES(CRNAME,'W',CRMESS)
	      ok=.false.
	   endif
	   
	   call map_get_int(fname, 'tag_e', 'tch_max', 384,
     +  	tchmax, irun, run0)
	   CRMESS='Reading tch_max for ET map from caldb'
	   CALL RECMES(CRNAME,'I',CRMESS)
	   if (run0.lt.0) then
	      CRMESS='Problem while reading tch_max for ET map from caldb'
	      CALL RECMES(CRNAME,'W',CRMESS)
	      ok=.false.
	   endif
	else ! <read from file>
	   if (ectcmapsMM.eq.0) then
	      if (where.eq.1) then
		 open (unit=Ilun,status='old',err=111,file='tagETcoinc_ALL.dat')
		 CRMESS='Reading E-T geometric matching from local file: tagETcoinc_ALL.dat'
	      else
		 call revinm('CLAS_PARMS','tagETcoinc_ALL.dat',fname)          
		 open (unit=Ilun,status='old',err=101,file=fname)
		 CRMESS='Reading energy E boundaries from file '//fname
	      endif
	      CALL RECMES(CRNAME,'I',CRMESS)
	      endfile = .FALSE.
	      BestEnergy = 0.
	      do while(.not.endfile)
c       ---------- find next energy
		 FoundEnergy = .FALSE.
		 Corrupted = .FALSE.
		 do while(.not.FoundEnergy)
		    read(Ilun,'(a)',END=501) line
		    if (index(line,'E0').ne.0) then
		       FoundEnergy = .TRUE.
		       read (line,'(a7,f6.4)') dummarray,NewEnergy 
		    endif
		 enddo
		 if (abs(newenergy-energy).lt.abs(bestenergy-energy)) then ! most trivial algorithm: take closer value
		    bestenergy = newenergy
c------------read values.
		    Corrupted = .TRUE. ! if loop ends before the end, detect corupted files
		    do i=1,384
 1001		       read(Ilun,'(a)',END=501) line ! read 80-character line
		       if(index(line,'#').ne.0) go to 1001 ! skip lines with '#'
		       read(line,'(7i5)') itest,tmin(i),tmax(i),
     +           	  t1min(i),t1max(i),tchmin(i),tchmax(i)
		       if(itest.ne.i) go to 501	! test for data corruption

c Apr-30-2008 EAP
c change lower limit for ET matching in transition region
		       if(tchmin(i).eq.38) tchmin(i)=37

c 13-MAR-2006 EAP
c       per indication from eg3
c       open up ET-map by 1 Tch for E0 above 4.5 and T-counter above 19 
		       if(energy .gt. 4.5 .and. tmin(i) .ge.19) tchmax(i) =tchmax(i)+1 
		       
		       if(Tagger_ETmatch.eq.0)then !turn of geometry matching
			  tmin(i)=1
			  tmax(i)=61
			  t1min(i)=1
			  t1max(i)=61
			  tchmin(i)=1
			  tchmax(i)=121
		       endif
		       tbinmin(tag_bin(i,1)) = tag_bin(tmin(i),1)
		       if(i.lt.384) tbinmin(tag_bin(i,2)) = tag_bin(tmin(i),1)
		    end do
		 endif		! if (abs().lt.abs())
		 goto 201
c--------------handle end of file
 501		 endfile=.TRUE.    
		 if (corrupted) then
		    CRMESS='E-T matching file corrupted !'
		    CALL RECMES(CRNAME,'W',CRMESS)
		    ok=.FALSE.
		 endif 
		 if (BestEnergy.eq.0) then
		    CRMESS='Could not find energy values for E-T matching'
		    CALL RECMES(CRNAME,'W',CRMESS)
		    ok=.FALSE.
		 else
		    write(CRMESS,*) ' E-T matching map found for energy ',BestEnergy,' Gev' 
		    call RECMES(CRNAME,'I',CRMESS) 
		    ectcmapsMM = 1
		 endif
 201	      enddo		! do while(.not.endfile)
	      close(Ilun)
	   endif		! if (ectccmapsMM.eq.0)
	endif			!	if(use_tgtlr .eq. 1 .and. et_map_from_db .eq. 1)
	
 18	Call ResLun(crname,-Ilun,Iretb)
        RETURN

 106	CRMESS='!! No T-counter boundaries file in CLAS_PARMS!!'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
	goto 17
 101	CRMESS='!! No E-T matching file in CLAS_PARMS!!!'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
	goto 18
 116	CRMESS='No local T-counter boundaries file'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
        goto 17
 111	CRMESS='No local E-T matching file'
        CALL RECMES(CRNAME,'W',CRMESS)
        ok=.FALSE.
        goto 18

        END
c
c------------------------------------------------------------------------------


