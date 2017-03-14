CDECK  ID>, FSEQST.
      SUBROUTINE FSEQST
c
c_begin_doc
c  RCS ID string
c  $Id: fseqst.F,v 1.2 1997/04/12 18:03:27 freyberg Exp $
c
c  Purpose:
c  --------
c        start = initialization of FSEQR parameters
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
caf#include "boscom.inc"
#include "bcs.inc"
#include "bossys.inc"
#include "bosmdl.inc"
#include "cseqr.inc"
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: fseqst.F,v 1.2 1997/04/12 18:03:27 freyberg Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FSEQST.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NAMIND,NCR,IND,NDEB
cboy      CHARACTER*4 CHAINT
cboy      CHARACTER*8 NAME
      LOGICAL START
      DATA START/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(.NOT.START) GOTO 40
   10 START=.FALSE.
      CALL TIMEX(TBEGIN)
*        initial entry, reset logical flags ...
      EOD =.FALSE.
      USTOP=.FALSE.
      URSKP=.FALSE.
*        previous and current run, current event number
      LCCRUN=0
      NCCRUN=0
      NEVENT=0
      NREC =0
      NCR =0
*        ... and logical user flags
      BEGJOB=.TRUE.
      ENDRUN=.FALSE.
      BEGRUN=.FALSE.
      REVENT=.FALSE.
      ENDJOB=.FALSE.
      OTHDAT=.FALSE.
*        ... further flags
      NLUNW=0
      ISPECL=0
      KEYNR1=0
      KEYNR2=0
      NAMKEY='        '
      NDATWD=0
      LIMSEC=2
      LIMREC=0
*  interpret steering cards NEVENT, TIMEEND and DEBUG
      IND=IW(NAMIND('NEVE'))
      IF(IND.NE.0)THEN
        LIMREC=IW(IND+1)
      ENDIF
      IND=IW(NAMIND('TIME'))
      IF(IND.NE.0)THEN
        LIMSEC=IW(IND+1)
      ENDIF
*        allow for several debug cards
      IND=NAMIND('DEBU')+1
   20 IND=IW(IND-1)
      IF(IND.NE.0)THEN
        NDEB=IW(IND)/4
        IF(MOD(IW(IND),4).NE.0)THEN
          WRITE(6,10000)
10000        FORMAT(' Wrong number of parameters in DEBUG command or ',
     +              'module name with < 5 letters - command ignored.')
        ELSE
 
cboy          INDX=IND
cboy          DO 30 I=1,NDEB
cboy            NAME = CHAINT(IW(INDX+1))//CHAINT(IW(INDX+2))
cboy            IDLEV = IW(INDX+3)
cboy            NEV = IW(INDX+4)
cboy            CALL SETDEB(NAME,IDLEV,NEV)
cboy            INDX=INDX+4
cboy   30     CONTINUE
 
        ENDIF
        GOTO 20
      ENDIF
*        reset lists ESR
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'S=','0')
      CALL BLIST(IW,'R=','0')
      GOTO 40
      ENTRY FSEQSF
*     forced (re)start
      GOTO 10
   40 RETURN
      END
