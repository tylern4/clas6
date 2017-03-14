      SUBROUTINE ST_EVNT(trk_level)
c
c_begin_doc
c  RCS ID string
c  $Id: st_evnt.F,v 1.17 2005/11/08 03:03:47 langhei Exp $
c
c  Documentation for subroutine ST_EVNT
c
c  Purpose:  ST supplied EVENT processing routine, called once per event
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
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - added st_guess_nhits call  :Burin Feb98
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
#include "sttcl.CMN"
#include "st_general.PAR"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
       logical ok
       integer trk_level
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: st_evnt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.17 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/11/08 03:03:47 $')
      PARAMETER ( CAUTHO = '$Author: langhei $')
      DATA CRCSID/
     1'$Id: st_evnt.F,v 1.17 2005/11/08 03:03:47 langhei Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='ST_EVNT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local St defined variables:
c  ---------------------------
       character*4 trk_result_BOS_name
       character*132 crmess
       integer nwrite
c_end_var
c
c  executable code for routine ST_EVNT:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a ST_EVNT routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Use C++ code and bypass the boring FORTRAN ? ...
      if (StType.eq.2) then
         call stpp_evnt(trk_level)
         return
      endif

c ... nope, FORTRAN it is
      if (trk_level.eq.HIT_BASED) then
         trk_result_BOS_name='HDPL'
      elseif (trk_level.eq.TIME_BASED) then
         trk_result_BOS_name='TDPL'
      else
         CRMESS='track level not implemented in FORTRAN code, try C++'
         CALL RECMES(CRNAME,'W',CRMESS)
      endif


c Drop reconstructed banks
        call bdrop(iw,'ST1 ')
        call bdrop(iw,'STR ')

      call st_reset_arrays

c
c   extract from BOS bank, tdc & adc. Store them in stdata common block
c
       call stRead_from_BOS(OK)
c
c
       if(.NOT.OK) then
c            CRMESS='Unable to correctly read ST bank'
c            call RECMES(CRNAME,'W',CRMESS)
            RETURN
       EndIf
c     perform time conversion based on the calibration
c
       if(StType.eq.0) then
          call st_calibrate(OK)
       else
          call stn_calibrate(OK)
       endif

       if(.NOT.OK) then
c            CRMESS='not able to convert raw tdcs to time'
c             call RECMES(CRNAME,'W',CRMESS)
            RETURN
       EndIf
c
c  store for further processing information that has been succesfully converted
c
        call stItm_fetch_to_BOS(OK)
c
c  extract from HDPL BOS bank how many tracks are in one pair
c  as seen by the drift chamber
       if(StType.eq.0) then
          call st_find_trk(OK,trk_result_BOS_name)
       else
          call stn_find_trk(OK,trk_result_BOS_name)
       endif
c
c
       if(.NOT.OK) then
c             CRMESS='Unable to extract info from HDPL BOS banks'
c             call RECMES(CRNAME,'W',CRMESS)
            RETURN
       EndIf
c
c  use info from ST+TRK to determine how many hits in each pair
c
       call st_guess_nhits
c
       if(StType.eq.0) then
          call st_time_walk
       else
          call stn_time_walk
       endif

c
c
c       if(.NOT.OK) then
c            crmess = 'problem during '
c            call RECMES(CRNAME,'W',CRMESS)
c            RETURN
c       EndIf
c
c   perform the final diagnostic of the start counter
c
       if(StType.eq.0) then
          call st_fit(OK)
       else
          call stn_fit(OK)
       endif

c
c
       if(.NOT.OK) then
             crmess = 'Not able to fit'
             call RECMES(CRNAME,'W',CRMESS)
            RETURN
       EndIf
c
c      store fit results into the start counter result bank
       call st_fetch_to_BOS(OK, TRK_LEVEL)
c
c
       if(.NOT.OK) then
             crmess = 'Could not put STR to BOS'
             call RECMES(CRNAME,'W',CRMESS)
            RETURN
       EndIf
c
       RETURN
       END
c
c----------------------------------------------------------------------




