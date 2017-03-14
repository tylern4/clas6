      SUBROUTINE TAG_PS_ANALYSIS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PS_analysis.F,v 1.1 1998/03/17 02:20:41 anciant Exp $
c
c  Documentation for subroutine TAG_PS_ANALYSIS
c
c  Purpose:  Pair spectrometer analysis routine, called once per event
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
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
c
c There are 4 scintillators on each sides which are a litle bit overlaping,
c 
c               L1 (1)  ---  /\   ---   R1 (5)
c                            ||                     
c           L2 (2)    ---   beam    ---   R2 (6)
c                            
c          L3 (3)   ---      ||       ---   R3 (7)
c                       
c  L4 (4)  ----------        ||         -----------  R4 (8)
c
c                            ||
c                            ==  converter
c
c   low paddle Id's are for high energy electrons/positrons
c   The left side measures the e+, the right side measures the e-
c   Left paddles are counters 1 to 4, right paddles are counters 5 to 8
c   for each event we can define a hit pattern, corresponding to the counters
c   that are hit in time.
c   
c  This pattern can be stored using a bit pattern:
c       bit 1 is for paddle 1, bit 2 for paddle 2, and so forth..
c
c  for example, if a pair is produced that hit paddles 2, 3 & 8
c  the hit-bit pattern would be : 10000110
c                                (87654321)
c  this pattern is saved in the variable ps_pattern
c  the corresponding event time is saved in the variable ps_time
c  
c  there can more than one event recorded for one DAQ record, for exemple,
c  a very high enery photon could hit paddles 1 & 6 and 50 ns later a lower energy
c  photon could leave hits in paddles 4 & 7.
c  For this reason ps_pattern and ps_time are arrays that can contain up to 4 events.
c
c  M.K & E.A

c  Author:   Eric Anciant, created march 98
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
c#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"     
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical OK
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_PS_analysis.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/03/17 02:20:41 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_PS_analysis.F,v 1.1 1998/03/17 02:20:41 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_PS_ANALYSIS')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       integer i,j,alreadypicked(8),Nbpaddle,Nb_Remove
       integer iand
       
c_end_var
c
c  executable code for routine TAG_PS_ANALYSIS
c  -------------------------------------
c
       crmess='  '

c
c----------------------------------------------------------------------
c     Select only those hits where the TDC is expected to lie.
c        Need to read in the values of PS_MIN_VALUE and PS_MAX_VALUE
c        (similar to what Luc has done for the E-counters and where
c        they should be in the TDC spectra)
c----------------------------------------------------------------------
c
!@    my personnal opinion is that removing those hits now might
!@    make us loose some interesting informations about accidentals,
!@    TDC calibration (width of one channel) and informations
!@    that could be used to measure the time walk corrections.
!@
!@     Nb_Remove = 0
!@
!@       do i=1,Nb_Hits_PS
!@          if (ps_scint_time(i) .lt. PS_MIN_VALUE .or. ps_scint_time(i)
!@     +         .gt. PS_MAX_VALUE) then
!@             ps_scint_time(i) = 0.0
!@             Nb_Remove = Nb_Remove + 1
!@          endif
!@       enddo
c     
c----------------------------------------------------------------------
c     Remove from the Nb_Hits_PS those that were not in the correct
c        place in the TDC spectrum for each event...
c----------------------------------------------------------------------
c
!@       Nb_Hits_PS = Nb_Hits_PS - Nb_Remove
!@
!@ If you finally decide to remove hits based on where they appear in TDC range
!@ then there is a little bug here, it is not enough to make this substraction
!@ one also have to put the (Nb_Hits_PS - Nb_Remove) good hits in the first
!@ elements of the array.

c
c  ---- PS advanced processing
c
             
       Nb_evnt_PS = 0
       do i = 1,Nb_Hits_PS
          alreadypicked(i) = 0
       enddo
       
       do i = 1,Nb_Hits_PS

!@          if (alreadypicked(i) .eq. 0) then
!@  this is where we should test if this first hit is a good one.
!@  + as far as I understand,
!@   if the DAQ is working perfectly then the pedestals will be removed
!@  and SUBSTRACTED inside the ROC before datas are stored,
!@  which means in a perfect word the SOFTWARE pedestals will be 0 !!
!@  so I would rather have an independent PSthreshold variable.
!@

          if (alreadypicked(i) .eq. 0.and.ps_scint_ene(i) .gt. PSthreshold) then
           
c
c----------------------------------------------------------------------
c     we don't analyse hits that are already part of a previous event
c     or that are noise in the TDC
c----------------------------------------------------------------------
c
             Nb_evnt_PS = Nb_evnt_PS + 1
             Nbpaddle = 1
c
c----------------------------------------------------------------------
c     Put the time of the event in ps_time for each hit in the PS
c        (maximum of 4) if it is in the right place in the TDC
c        spectrum which is determined above...
c----------------------------------------------------------------------
c
             ps_time(Nb_evnt_PS) = ps_scint_time(i) !@ this is the beginning of the 
                                                    !@ sum that will be averaged
             ps_pattern(Nb_evnt_PS) = 2**ps_id(i)
             do j = i+1,Nb_Hits_PS
!@                if (alreadypicked(j) .eq. 0) then
                if (alreadypicked(j) .eq. 0.and.ps_scint_ene(j) .gt. PSthreshold) then

!@c
!@c----------------------------------------------------------------------
!@c     we don't analyse hits that are already part of a previous event
!@c     or that are noise in the TDC
!@c----------------------------------------------------------------------
!@c

                   if (abs(ps_scint_time(i)-ps_scint_time(j))
     +                  .lt. PS_COINC_WINDOW) then
!@c
!@c----------------------------------------------------------------------
!@c     time coincidence match found.   Let's look at the ADC spectrum
!@c         and see if we have a sizable hit above pedestal.   Place
!@c         into the alreadypicked array, code the hit pattern into
!@c         a number, (0-127), and place into ps_pattern array.
!@c
!@c     FACTOR is a number to check to see if a hit in the ADC is well
!@c         above the pedestal value.  I don't have a clear idea about
!@c         its size or whether this is the best way to do the selection.
!@c----------------------------------------------------------------------
!@c
!@                     if ( ps_ped(ps_id(i)).gt. ps_ped(ps_id(i))*FACTOR
!@     +                     .and. ps_scint_ene(j) .gt. ps_ped(ps_id(j))
!@     +                     *FACTOR) then
!@
!@c
!@c----------------------------------------------------------------------
!@c     Chances are this was a real event and not a fluke...
!@c----------------------------------------------------------------------
!@c
                         alreadypicked(j) = 1
                         ps_pattern(Nb_evnt_PS) = ps_pattern(Nb_evnt_PS)+
     +                        2**ps_id(j)
c
c----------------------------------------------------------------------
c     This is a very crude/temporary analysis.   I am working on
c         a better scheme for this analysis.
c----------------------------------------------------------------------
c

c
c     Here goes better timing code...
c

                         Nbpaddle = Nbpaddle + 1
                         ps_time(Nb_evnt_PS) = ps_time(Nb_evnt_PS)+
     +                        ps_scint_time(j)
!@                    endif
                   endif
                endif
             enddo
c
c----------------------------------------------------------------------
c     Now average the timings over the number of paddles found...
c----------------------------------------------------------------------
c
             ps_time(Nb_evnt_PS) = ps_time(Nb_evnt_PS)/Nbpaddle
          endif
       enddo




c
c     Does this go here???
c

c----------------------------------------------------------------------
c     Require that a good event be such that a left and right side
c         paddle fired (regardless of how many).   A more sophisticated
c         section of code will be needed to determine what these events
c         are telling us...
c----------------------------------------------------------------------
c
       do i = 1,Nb_evnt_PS
!@          if (iand(ps_pattern(i),'0F'x) .eq. 0 .or.
!@     +         iand(ps_pattern(i),'F0'x) .eq. 0) then
c
c----------------------------------------------------------------------
c     How should we handle such a case?   Should we consider the event
c        invalid?
c----------------------------------------------------------------------
c
!@
!@  To start with, I would suggest to keep everything
!@  and look at real data first, then it is time to decide....
!@
!@          endif
       enddo
c
       RETURN
       END
c
c----------------------------------------------------------------------