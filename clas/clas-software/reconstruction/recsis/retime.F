      SUBROUTINE RETIME
c
c_begin_doc
c  RCS ID string
c  $Id: retime.F,v 1.1 1997/03/28 20:58:17 marki Exp $
c
c  Documentation for subroutine RETIME
c
c  Purpose:  Prints out CPU time information
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
c  Author:   Arne Freyberger      Created:  Sun Mar 19 10:16:46 EST 1995
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
      PARAMETER ( CFILE = '$RCSfile: retime.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/03/28 20:58:17 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'
     2$Id: retime.F,v 1.1 1997/03/28 20:58:17 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RETIME')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I
      CHARACTER*20  CTALLY(NTRIG)
      DATA CTALLY/15*'Physics Event',
     1               'Sync Event',
     2               'Begin Run',
     3               'Go Event',
     4               'Pause Event',
     5               'End Run',
     6               'Unknown Trigger'/
c_end_var
c
c  executable code for routine RETIME:
c  -------------------------------------
c
      CALL RECMES(CRNAME,'N','   ')
      DO 10 I=1,NTRIG
       IF (ITALLY(I)+ITALLY_MISSED(I) .EQ. 0) GOTO 10
       WRITE(CRMESS,900) CTALLY(I), CPUSUM(I)
       CALL RECMES(CRNAME,'N',CRMESS)
10    CONTINUE
      CALL RECMES(CRNAME,'N','   ')
      DO 20 I=1,NTRIG
       IF (ITALLY(I)+ITALLY_MISSED(I) .EQ. 0) GOTO 20
       WRITE(CRMESS,910) CTALLY(I), 
     1       CPUSUM(I)/(ITALLY(I)+ITALLY_MISSED(I))*1000.
       CALL RECMES(CRNAME,'N',CRMESS)
20    CONTINUE
      CALL RECMES(CRNAME,'N','   ')
900   FORMAT('Time spent in ',A16,
     1       ' processing was ',F6.3,' seconds')
910   FORMAT('Average time spent ',A16,
     1       ' processing was ',F6.3,' milliseconds')
      RETURN
      END
c
c------------------------------------------------------------------------------




