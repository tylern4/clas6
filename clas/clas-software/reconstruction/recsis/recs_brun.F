      SUBROUTINE RECS_BRUN
c
c_begin_doc
c  RCS ID string
c  $Id: recs_brun.F,v 1.4 2000/04/03 19:05:21 stepanya Exp $
c
c  Documentation for subroutine RECS_BRUN
c
c  Purpose: Performs BEGIN RUN tasks for the RECSIS package. Called after
c  -------- ALL other Begin Run processing is done.
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
c  Author:   Arne Freyberger      Created:  Tue Apr 11 13:27:56 EDT 1995
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
#include "clasmdl.inc"
#include "bnkfmt.inc"
#include "wcs.inc"         
#include "bcsparm.inc"     
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
      PARAMETER ( CFILE = '$RCSfile: recs_brun.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2000/04/03 19:05:21 $')
      PARAMETER ( CAUTHO = '$Author: stepanya $')
      DATA CRCSID/
     1'$Id: recs_brun.F,v 1.4 2000/04/03 19:05:21 stepanya Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECS_BRUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*7 CVTYPE
      CHARACTER*26 CRTIM
      INTEGER LENOCC
c_end_var
c
c  executable code for routine RECS_BRUN:
c  -------------------------------------
c
c  ALWAYS WRITE BEGIN RUN RECORD!!!!
c
c      CALL FSEQW('CLASOUTPUT')
c
c  write out if this is a DATA, or MC run
c   
      IF (EVTYPE .EQ. -3) THEN
       CVTYPE = 'ClasSim'
      ELSEIF (EVTYPE .EQ. -2) THEN
       CVTYPE = 'GEANT  '
      ELSEIF (EVTYPE .EQ. -1) THEN
       CVTYPE = 'SDA    '
      ELSE
       CVTYPE = 'DATA   '
      ENDIF
C
      WRITE(CRMESS,10)NCCRUN,CVTYPE
10    FORMAT('Begin Run processing for run',I6,'  consisting of ',A6,
     1       '  events')
      CALL RECMES(CRNAME,'L',CRMESS)
C
      CALL getasciitime(EVTIME,CRTIM)
      WRITE(CRMESS,15)CRTIM(12:20),CRTIM(1:3),CRTIM(9:10),
     1                CRTIM(5:7),CRTIM(21:24)
15    FORMAT('Run was created at: ',A9,'on ',A3,' the ',
     1                A2,'th of ',A3,' ',A5)
      CALL RECMES(CRNAME,'L',CRMESS)
c
c
c  call REVERS to write DBBK head for this RUN
c
      call rejob_info
c
      CALL REVERS
c
c   call REDBBK to dump most recent and previous DBBK bank to log file
c
      CALL REDBBK(1)
c     update the write/not-write and memory delete list
c     
c     cbank_write contains the string of banks to write out
      
      IF (IWRITE .NE. 0)  
     1     CALL BLIST(IW,'E+',CBANK_WRITE(1:LENOCC(CBANK_WRITE)))
      
c     cbank_remove contains the string of banks NOT to be written out
      
      IF (IREMOVE .NE. 0)  
     1     CALL BLIST(IW,'E-',CBANK_REMOVE(1:LENOCC(CBANK_REMOVE)))
      
c     cbank_delete contains the string of banks to be deleted from memory
c     before the next event is read in
      
      IF (IDELETE .NE. 0)  
     1     CALL BLIST(IW,'R+',CBANK_DELETE(1:LENOCC(CBANK_DELETE)))
c
      RETURN
      END
c
c------------------------------------------------------------------------------




