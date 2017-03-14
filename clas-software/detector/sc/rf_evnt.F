      SUBROUTINE RF_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: rf_evnt.F,v 1.4 1999/03/09 17:56:05 boiarino Exp $
c
c  Documentation for subroutine RF_EVNT
c
c  Purpose:  RF event-based main routine
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
c     Author:   Eric Anciant,     april 8th, 1998
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_do
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "rfcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: rf_evnt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/09 17:56:05 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: rf_evnt.F,v 1.4 1999/03/09 17:56:05 boiarino Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='RF_EVNT')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess

        logical OK
        
        integer idCL01
	integer idCall
	integer MLINK
        integer rf1raw,rf2raw
        real rf1,rf2,goodrf,rf_corr
        integer MBANK,ind

        real RF_STRUCTURE
        parameter (RF_STRUCTURE=2.004)


c_end_var
c
c  executable code for routine RF_EVNT :
c  ------------------------------------
c
       crmess = '  '
       
c
c      store in common raw data contained in CALL BOS bank
c       
	idCall = MLINK(IW,'CALL',0)

       OK = (idcall.gt.0)
       if (.not.OK) then
c
c  If call does not exist I make the RF from the raw banks (97 photons runs patch)
c
         IF (NWRITE .LT. 1) THEN
           NWRITE = NWRITE + 1
           crmess = 'First rf reconstruction event'
           CALL RECMES(CRNAME,'I',CRMESS)
         ENDIF

          call rf_evnt_from_raw(OK,rf1raw,rf2raw)

          rf1 = float(rf1raw) * rf1channelW
          rf2 = float(rf2raw) * rf2channelW

      else
c
c  Get rf information from CL01 bank. 
c

          IF (NWRITE .LT. 1) THEN
            NWRITE = NWRITE + 1
            crmess = 'First rf reconstruction event, this message written once.'
            CALL RECMES(CRNAME,'I',CRMESS)
          ENDIF
      
c
c     check to see if CL01 bank exists
c
	idCL01 = MLINK(IW,'CL01',0)	  
        if (idCL01 .le. 0) then
           call make_cl01_bank
           idCL01 = MLINK(IW,'CL01',0)
           if (idCL01 .le. 0) then
              crmess = '*** idCL01 Bank cannot be created'
              call recmes (crname,'F',crmess)
              stop ' *** rf_evnt - idCL01 Bank cannot be created'
           endif
        endif
c
c       get information from CL01 bank via subroutine
c
        call rf_numbers (rf1,rf2,goodrf,rf_corr)
c
      endif
c
c      Fill RF bank
c     
       if (OK) then

c       put the output in the RF bank:
c
        ind = MBANK(IW,'RF  ',0,3,1)

        RW(ind + 1) = goodrf
        RW(ind + 2) = rf1
        RW(ind + 3) = rf2
        
       endif ! if (OK)
       
       RETURN
       END
c
c----------------------------------------------------------------------





