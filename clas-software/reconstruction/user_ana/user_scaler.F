      SUBROUTINE USER_SCALER
c
c_begin_doc
c  RCS ID string
c  $Id: user_scaler.F,v 1.4 2002/03/20 22:21:20 stepanya Exp $
c
c  Documentation for subroutine USER_SCALER
c
c  Purpose: Called for event type 0 events which contain scaler data and
c  -------- other special data type
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
c  Author:   Arne Freyberger      Created:  Thu Oct  9 15:55:30 EDT 1997
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
#include "run_control.inc"
#include "histtcl.inc"
#include "user_control.inc"
#include "bnkfmt.inc"
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
      PARAMETER (CFILE = '$RCSfile: user_scaler.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/03/20 22:21:20 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: user_scaler.F,v 1.4 2002/03/20 22:21:20 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_SCALER')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,I,NS,ierr
      INTEGER LENOCC
c_end_var
c
c  executable code for routine USER_SCALER:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
 
      DO i=1,10
        IF(outbanknames(i).ne.'junk')THEN
          ns=i+1
          call fwbos(iw,ns,CBANK_WRITE(1:LENOCC(CBANK_WRITE)),ierr)
        ENDIF      
      ENDDO
c
c       CALL FSEQW('CLASOUTPUT')
c change back to top directory       
       call hcdir('//PAWC',' ')
       call hcdir('//LUN1',' ')

c
      RETURN
      END
c
c------------------------------------------------------------------------------




