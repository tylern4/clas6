      SUBROUTINE RECPU(IFLAG)
c
c_begin_doc
c  RCS ID string
c  $Id: recpu.F,v 1.1 1997/03/28 20:58:12 marki Exp $
c
c  Documentation for subroutine RECPU
c
c  Purpose:  Keeps running sum of CPU time spend in each of the SIX job 
c  --------  conditions: begin job, begin run, event, end run, end job and
c            unknown event type.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (IFLAG - I - Trigger type)
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
c  Author:   Arne Freyberger      Created:  Sun Mar 19 10:16:42 EST 1995
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
#include "clasmdl.inc"
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
      PARAMETER ( CFILE = '$RCSfile: recpu.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/03/28 20:58:12 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'
     2$Id: recpu.F,v 1.1 1997/03/28 20:58:12 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECPU')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL INTRAC
      EXTERNAL INTRAC
c
      INTEGER I, IFLAG
      REAL TDEL, TNEW, TOLD
c_end_var
c
c  executable code for routine RECPU:
c  -------------------------------------
c
c
c       IF (INTRAC()) THEN
c         CALL TIMEST(1.0E10)
c         LINTER = .TRUE.
c       ELSE
c         LINTER = .FALSE.
c       ENDIF
c
       TOLD = TNEW
       CALL TIMEX(TNEW)
       TDEL = TNEW - TOLD
c
       IF (IFLAG .EQ. 0) THEN
        DO 10 I=1,NTRIG
         IF (PHYSTYP(I) .EQ. 1) THEN
          CPUSUM(I) = CPUSUM(I) + TDEL
         ENDIF
10      CONTINUE
       ELSE
        CPUSUM(IFLAG) = CPUSUM(IFLAG) + TDEL
       ENDIF
       RETURN
       END
c
c------------------------------------------------------------------------------




