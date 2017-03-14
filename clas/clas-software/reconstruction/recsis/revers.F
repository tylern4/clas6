       SUBROUTINE REVERS
c
c_begin_doc
c  RCS ID string
c  $Id: revers.F,v 1.2 1999/03/09 17:18:49 boiarino Exp $
c
c  Documentation for subroutine REVERS
c
c  Purpose:  At each BEGIN RUN fills the databank DBBK  bos bank with 
c  --------  version numbers and other run associated information
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters: 
c  ----------------
c
c  Output Parameters:
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
c  Author:   Arne Freyberger      Created:  Mon Apr 10 12:24:42 EDT 1995
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
#include "reversion.inc"
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
      PARAMETER ( CFILE = '$RCSfile: revers.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/09 17:18:49 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: revers.F,v 1.2 1999/03/09 17:18:49 boiarino Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REVERS')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*4 CHAINT
      INTEGER MLINK, MBANK, INTCHA
      EXTERNAL MLINK, MBANK, INTCHA, CHAINT
c
      INTEGER IND, ILAST, IPASS, NREC, NDATA, INDU, NR, I
      CHARACTER*20 CFMT
      CHARACTER*20 CTEMP
      DATA CFMT/'16A,3I,274A'/
      DATA NDATA/231/
c_end_var
c
c  executable code for routine REVERS:
c  -------------------------------------
c
c  First see if DBBK exists, if so get the information and update it
c  if not create it and fill it.
c
      IND = 0
      IPASS = 0
      IND = MLINK(IW,'DBBK',NR)
      IF (IND .NE. 0) THEN 
10     IPASS = IPASS + 1
       ILAST = IND
       IND = IW(IND-1)
       IF (IND .NE. 0)  GOTO 10
       NREC = IPASS + 1
       IPASS = IPASS + 1
      ELSE
       IPASS = 1
c
c  create DBBK format and add it to the output list
c
       CALL MKFMT(IW,'DBBK',CFMT)
       CALL BLIST(IW,'E+','DBBK')
       NREC = 0 
      ENDIF
         INDU = MBANK(IW,'DBBK',NREC,NDATA,1)
         IF (INDU .NE. 0) THEN
c
c  The information is packed in two four letter peices
c
           IW(INDU+1) = INTCHA(JOBDATE(1:4))
           IW(INDU+2) = INTCHA(JOBDATE(5:8))
           IW(INDU+3) = INTCHA(JOBDATE(9:12))
           IW(INDU+4) = INTCHA(JOBDATE(13:16))
           IW(INDU+5) = INTCHA(JOBUSRNM(1:4))
           IW(INDU+6) = INTCHA(JOBUSRNM(5:8))
           IW(INDU+7) = INTCHA(JOBEXE(1:4))
           IW(INDU+8) = INTCHA(JOBEXE(5:8))
           IW(INDU+9) = INTCHA(JOBMACH(1:4))
           IW(INDU+10) = INTCHA(JOBMACH(5:8))
           IW(INDU+11) = INTCHA(GLOBVER(1:4))
           IW(INDU+12) = INTCHA(GLOBVER(5:8))
           DO 13 I= 1,10
            IW(INDU+12+I) = INTCHA(INPUTDEV(4*(I-1)+1:4*I))
            IW(INDU+22+I) = INTCHA(OUTPUTDEV(4*(I-1)+1:4*I))
            IW(INDU+32+I) = INTCHA(JOBSCRIPT(4*(I-1)+1:4*I))
13         CONTINUE
           IW(INDU+43) = NPACKAGES
	   IW(INDU+44) = NSEQ
           IW(INDU+45) = IPASS
           DO 25 I=1,MIN(NPACKAGES,MAXPACK)
            IW(INDU+7*(I-1)+46) = INTCHA(CPACK(I))
            IW(INDU+7*(I-1)+47) = INTCHA(VPACK(I))
            CTEMP = DPACK(I)
            IW(INDU+7*(I-1)+48) = INTCHA(CTEMP(1:4))
            IW(INDU+7*(I-1)+49) = INTCHA(CTEMP(5:8))
            IW(INDU+7*(I-1)+50) = INTCHA(CTEMP(9:12))
            IW(INDU+7*(I-1)+51) = INTCHA(CTEMP(13:16))
            IW(INDU+7*(I-1)+52) = INTCHA(CTEMP(17:20))
25         CONTINUE
         ELSE
          CALL RECMES(CRNAME,'W','Cannot update database bank')
         ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




