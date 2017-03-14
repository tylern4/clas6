      SUBROUTINE REDBBK(NDUMP)
c
c_begin_doc
c  RCS ID string
c  $Id: redbbk.F,v 1.3 1999/03/09 17:18:49 boiarino Exp $
c
c  Documentation for subroutine REDBBK
c
c  Purpose:  outputs the DBBK bos bank to the log file in a user friendly way
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (NDUMP - I - 0 dumps most recent (last) DBBK bank)
c  ----------------                1       two most recent DBBK bank
c                                  2       three
c
c
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
c  Author:   Arne Freyberger      Created:  Tue Apr 11 10:22:14 EDT 1995
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
      PARAMETER ( CFILE = '$RCSfile: redbbk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/09 17:18:49 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: redbbk.F,v 1.3 1999/03/09 17:18:49 boiarino Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REDBBK')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK
      CHARACTER*4 CHAINT
      EXTERNAL CHAINT, MLINK
c
      INTEGER IND(100),JND, NDUMP, IPASS, NR, NPACK, ILAST, I
      INTEGER IBANK
c      CHARACTER*8 C8
      CHARACTER*16 C16
c_end_var
c
c  executable code for routine REDBBK:
c  -------------------------------------
c
c  First see if DBBK exists, if so get the information and update it
c  if not create it and fill it.
c
      IBANK = 0
      IBANK = IBANK + 1
      IND(IBANK) = 0
      IND(IBANK) = MLINK(IW,'DBBK',NR)
      IF (IND(IBANK) .NE. 0) THEN
10     ILAST = IND(IBANK)
       IND(IBANK+1) = IW(IND(IBANK)-1)
       IF (IND(IBANK+1) .NE. 0)  THEN
         IBANK = IBANK+1
         GOTO 10
       ENDIF
c
c  found ALL the DBBK banks, the indices are in IND(I) i=1,ibank
c  now loop through them start with the last bank (most recent temporally)
        WRITE(CRMESS,90)
90      FORMAT(54('-'))
        CALL RECMES(CRNAME,'I',CRMESS)
       DO 30 JND = IBANK,MAX(1,IBANK-NDUMP),-1
        C16(1:4)   = CHAINT(IW(IND(JND)+1))
        C16(5:8)   = CHAINT(IW(IND(JND)+2))
        C16(9:12)  = CHAINT(IW(IND(JND)+3))
        C16(13:16) = CHAINT(IW(IND(JND)+4))
        IPASS = IW(IND(JND)+45)
        IF (JND .EQ. IBANK) THEN
          WRITE(CRMESS,80)IPASS
80        FORMAT('This is pass ',I2)
          CALL RECMES(CRNAME,'N',CRMESS)
          WRITE(CRMESS,93)C16(1:8),C16(9:16)
93        FORMAT('Processing date for this pass is : ',A8,2x,A8)
        ELSEIF (JND .EQ. IBANK-1) THEN
          WRITE(CRMESS,85)IPASS
85        FORMAT('The most previous pass was pass',I2)
          CALL RECMES(CRNAME,'N',CRMESS)
          WRITE(CRMESS,95)C16(1:8),C16(9:16)
95        FORMAT('Processing date for previous pass was : ',A8,2x,A8)
        ELSE
          WRITE(CRMESS,100)IPASS,C16(1:8),C16(9:16)
100       FORMAT('Processing date for pass ',I2,' was : ',A8,2x,A8)
        ENDIF
        CALL RECMES(CRNAME,'N',CRMESS)
        NPACK = IW(IND(JND)+43)
        IF (JND .EQ. IBANK) THEN
         WRITE(CRMESS,200)NPACK
200      FORMAT('There are ',I2,'  packages initialized')
        ELSE
         WRITE(CRMESS,205)NPACK
205      FORMAT('There were ',I2,'  packages initialized')
        ENDIF
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,210)
210     FORMAT('Package',8x,'RCS version',8x,'RCS date')
        CALL RECMES(CRNAME,'N',CRMESS)
        if (npack .lt. 25) then
        DO 20 I = 1,NPACK
         WRITE(CRMESS,250)CHAINT(IW(IND(JND)+7*(I-1)+46)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+47)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+48)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+49)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+50)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+51)),
     1                    CHAINT(IW(IND(JND)+7*(I-1)+52))
250      FORMAT(A4,10X,A4,15X,A4,A4,A4,A4,A4)
         CALL RECMES(CRNAME,'N',CRMESS)
20      CONTINUE 
        endif
        WRITE(CRMESS,50)IW(IND(JND)+44)
50      FORMAT('Job sequence number: ',t25,I4)
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,60)(CHAINT(IW(IND(JND)+12+I)),I=1,10)
60      FORMAT('Input device: ',t25,10(A4))
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,61)(CHAINT(IW(IND(JND)+22+I)),I=1,10)
61      FORMAT('Output device: ',t25,10(A4))
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,65)(CHAINT(IW(IND(JND)+32+I)),I=1,10)
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,66) CHAINT(IW(IND(JND)+7)),CHAINT(IW(IND(JND)+8))
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,67) CHAINT(IW(IND(JND)+9)),CHAINT(IW(IND(JND)+10))
        CALL RECMES(CRNAME,'N',CRMESS)
65      FORMAT('Processing script: ',t25,10(A4))
66      FORMAT('Executible name: ',t25,2(A4))
 67     FORMAT('Machine name: ',t25,2(A4))
        WRITE(CRMESS,70)CHAINT(IW(IND(JND)+5)),CHAINT(IW(IND(JND)+6))
70      FORMAT('Person responsible: ',t25,A4,A4)
        CALL RECMES(CRNAME,'N',CRMESS)
        WRITE(CRMESS,90)
        CALL RECMES(CRNAME,'I',CRMESS)
30     CONTINUE
       WRITE(CRMESS,90)
       CALL RECMES(CRNAME,'I',CRMESS)
      ELSE
       CALL RECMES(CRNAME,'W','DBBK bank does not exists!')
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




