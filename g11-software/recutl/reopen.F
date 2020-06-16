      SUBROUTINE REOPEN(FILNAM,IUNIT,CSTATUS,IRET)
c
c_begin_doc
c  RCS ID string
c  $Id: reopen.F,v 1.3 1998/08/19 14:32:33 marki Exp $
c
c  Documentation for subroutine REOPEN
c
c  Purpose:  Opens file FILNAM with unit IUNIT and status CSTATUS
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 13:16:48 EST 1995
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
      PARAMETER (CFILE=  '$RCSfile: reopen.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/08/19 14:32:33 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: reopen.F,v 1.3 1998/08/19 14:32:33 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*150  CRMESS
      PARAMETER (CRNAME='REOPEN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ICEND, LENOCC
      EXTERNAL LENOCC
      CHARACTER*(*) FILNAM, CSTATUS
      INTEGER IUNIT, IRET
c_end_var
c
c  executable code for routine REOPEN:
c  -------------------------------------
c
c  First check the Logical unit is FREE
c
      CALL CHKLUN(IUNIT,IRET)
      IF (IRET .EQ. -1) THEN
       WRITE(CRMESS,10)IUNIT
       CALL RECMES(CRNAME,'W',CRMESS)       
      ELSEIF (IRET .EQ. -2) THEN
       WRITE(CRMESS,20)IUNIT
       CALL RECMES(CRNAME,'W',CRMESS)       
      ELSE
       ICEND = LENOCC(FILNAM)
       OPEN (UNIT=IUNIT, FILE=FILNAM(1:ICEND), STATUS=CSTATUS, ERR=999)
       WRITE(CRMESS,30)FILNAM(1:ICEND)
       CALL RECMES(CRNAME,'I',CRMESS)       
       IRET = 1
      ENDIF
      RETURN
999   CONTINUE
      IRET = -1
      WRITE(CRMESS,40)FILNAM(1:ICEND)
      CALL RECMES(CRNAME,'W',CRMESS)       
10    FORMAT('Can not open unit: ',I2,' unit already in use')
20    FORMAT('Can not open unit: ',I2,' unit already in use by FPACK')
30    FORMAT('File: ',A,' opened successfully')
40    FORMAT('File: ',A,' NOT opened ERROR upon opening')
      RETURN
      END
c
c------------------------------------------------------------------------------




