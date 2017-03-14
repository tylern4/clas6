      SUBROUTINE USER_EPICS
c
c_begin_doc
c  RCS ID string
c  $Id: user_epics.F,v 1.2 2008/03/16 04:33:28 fklein Exp $
c
c  Documentation for subroutine USER_EPICs
c
c  Purpose: Called for event type >=100 events which contain epics
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
      PARAMETER (CFILE = '$RCSfile: user_epics.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:28 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: user_epics.F,v 1.2 2008/03/16 04:33:28 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_EPICS')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,MAMIND
      external mamind
      INTEGER LENOCC
      INTEGER ind,namind,ncol,nrow,nd,indx,i,ns,ierr
      DATA namind/0/
c_end_var
c
c  executable code for routine USER_EPICS:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c      CALL FSEQW('CLASOUTPUT')
c
      do i=1,151
c         print *,i,epics_value(i),'    ',epics_name(i)
      enddo
      DO i=1,10
         IF(outbanknames(i).ne.'junk')THEN
            ns=i+1
            call fwbos(iw,ns,CBANK_WRITE(1:LENOCC(CBANK_WRITE)),ierr)
         ENDIF      
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------




