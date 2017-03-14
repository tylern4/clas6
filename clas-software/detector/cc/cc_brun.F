      SUBROUTINE CC_BRUN(irun)
c_begin_doc
c  RCS ID string
c  $Id: cc_brun.F,v 1.15 2005/11/23 20:56:57 vlassov Exp $
c
c  Documentation for subroutine CC_BRUN
c
c  Purpose:  Initialize CCRC code from the map database
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov     Created:  Fri Jun 21 18:13:28 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
c
c_begin_inc
c  include files :
c  ---------------------

#include "run_control.inc"
#include "cccalb.inc"

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
      PARAMETER (CFILE = '$RCSfile: cc_brun.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.15 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2005/11/23 20:56:57 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_brun.F,v 1.15 2005/11/23 20:56:57 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='CC_BRUN')
      PARAMETER (CRAUTH='Alexander Vlassov')
c 
c  Input variables:
c  ----------------
      integer irun
c
c  Local User defined variables:
c  -----------------------------
c
c_end_var
c
c  executable code for routine CC_BRUN:
c----6-------------------------------------------------------------72
c
c     get the run number
c     call the map database and get new pedestals
c
      call cc_read_map(irun, ccpedes,'CC_CALIB','pedestals','mean')
      call cc_read_map(irun, opheamp,'CC_CALIB','photoE','amplitude')
      call cc_read_map(irun, acctime,'CC_CALIB','TDC','T0')
      call cc_read_map(irun, xcct2ch,'CC_CALIB','TDC','T1')
c
      if(irun .lt. 48000) then
        itdc_min = 10
        itdc_max = 4000
        iadc_min = 50
        iadc_max = 8000
      else
        itdc_min = 10
        itdc_max = 12000
        iadc_min = 50
        iadc_max = 8000
      end if
c
      write(crmess, 32) irun
      call recmes(crname,'I',crmess)
32    FORMAT('Loading CC Calibration Database Values for Run:',
     & 1x,i4,1x)
c
      RETURN
      END
c
c-------------------------------------------------------------------




