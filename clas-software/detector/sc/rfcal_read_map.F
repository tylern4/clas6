      SUBROUTINE RFCAL_READ_MAP(OK,irun)
c
c_begin_doc
c  RCS ID string
c  $Id: rfcal_read_map.F,v 1.3 1999/03/09 17:56:06 boiarino Exp $
c
c  Documentation for subroutine RFCAL_READ_MAP
c
c  Purpose: Reads RF calibration constants in the Map Manager file.
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters:  Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   E. Anciant 	april 8th,  1998
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
#include "rfcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
        logical OK
        integer irun


c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: rfcal_read_map.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:56:06 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: rfcal_read_map.F,v 1.3 1999/03/09 17:56:06 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RFCAL_READ_MAP')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*99  call_map
      REAL value(10)
      INTEGER NWRITE
      integer flag

c_end_var
c
c  executable code for routine BUILDER_BRUN:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS=' '
c        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
c      call rernev(irun,ievnt,irec)
c        irun is a calling parameter

 
      call revinm('CLAS_PARMS','Maps/CALL_CALIB.map',call_map)
      call map_get_float(call_map,'T1', 'value',10 , value, irun, flag)
      rf1channelW = value(6)
      rf2channelW = value(7)
      call map_get_float(call_map,'RFoffset','rf2rf1Offset',1,rf2rf1Offset, irun, flag)

      RETURN
      END
c
c---------------------------------------------------------------------------
