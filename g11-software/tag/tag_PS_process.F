      SUBROUTINE TAG_PS_PROCESS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PS_process.F,v 1.4 1999/03/12 14:40:15 anciant Exp $
c
c  Documentation for subroutine TAG_PS_PROCESS
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
      PARAMETER ( CFILE = '$RCSfile: tag_PS_process.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/12 14:40:15 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_PS_process.F,v 1.4 1999/03/12 14:40:15 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_PS_PROCESS')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       integer i
       
c_end_var
c
c  executable code for routine TAG_PS_PROCESS
c  -------------------------------------
c
       crmess='  '
       
       IF (NWRITE .LT. 1) THEN
          NWRITE = NWRITE + 1
          crmess='analysis of Pair Spectrometer data'
          CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

c
c  ---- PS Basic processing
c

       if (Nb_Hits_PS.gt.0) then
         do i=1,Nb_Hits_PS
           
           ps_scint_ene(i) = ps_adc(i) - ps_ped(ps_id(i))
c
c----------------------------------------------------------------------
c     Is there a routine in the tagger code which allows for access
c         of map manager/Eric's parameter file for things such as
c         time-walks corrections?
c----------------------------------------------------------------------
c
           ps_scint_time(i) = 
     &          (ps_tdc_w(ps_id(i))/1000.)*float(ps_tdc(i))
     &          -posPSpeak(ps_id(i))

	ps_scint_time(i) = ps_scint_time(i) - TagTCiMean + tag2tof 	
	
c  apply same time offsets to DSD than to tagger T counters, i.e tagTCiMean, and tag2tof
c  for consistency.... E.A. 3/99
c

c
c----------------------------------------------------------------------
c     This is the equational form that Simon Taylor uses for the
c         Start Counter.   Since we have next to no information
c         from previous runs,   these parameters would only be guesses
c
c     Has there been any work with the PC about parameterizing the
c         the time walk corrections?   These might be a reasonable
c         start for the PS.   These parameters would be gotton from
c         the map or from Eric's parameter file.
c----------------------------------------------------------------------
c
           ps_scint_time(i) = ps_scint_time(i) + timwk1 + timwk2
     &          /ps_adc(i)**(timwk3)
           
         enddo
       endif
c
       RETURN
       END
c
c----------------------------------------------------------------------
