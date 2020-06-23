      SUBROUTINE REHIST
c
c_begin_doc
c  RCS ID string
c  $Id: rehist.F,v 1.2 1998/05/05 19:21:42 marki Exp $
c
c  Documentation for subroutine REHIST
c
c  Purpose:  dumps histogram to screen or log file
c  --------
c
c  Input Parameters:  
c  ----------------
c
c  Output Parameters: 
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Mar 21 09:35:40 EST 1996
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
#include "rhist.inc"
#include "re_units.inc"
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
      PARAMETER (CFILE = '$RCSfile: rehist.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/05/05 19:21:42 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rehist.F,v 1.2 1998/05/05 19:21:42 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REHIST')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
       character*132 pres_dir
c_end_var
c
c  executable code for routine REHIST:
c----6----------------------------------------------------------------72
c
c  find present directory  pres_dir
c
       CALL HCDIR(PRES_DIR,'R')
c
c  change to requested directory (hdir)
c
      call hcdir(hdir,' ')
c
c print out the id  (hid) to the terminal
c 
      call houtpu(lout)
      CALL HPRINT(HID)
c
c print out the histogram to the log file
c
      call houtpu(llog)
      CALL HPRINT(HID)
c
c  change back to the directory you were in when you entered
c
       CALL HCDIR(PRES_DIR,' ')
c
      RETURN
      END
c
c------------------------------------------------------------------------------




