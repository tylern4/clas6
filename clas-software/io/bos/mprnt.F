CDECK  ID>, MPRNT.
      INTEGER FUNCTION MPRNT(JW,NAME,NR)
c
c_begin_doc
c  RCS ID string
c  $Id: mprnt.F,v 1.1.1.1 1997/03/28 20:49:38 marki Exp $
c
c  Purpose:
c  --------
c     PRINT BANK (NAME,NR) AND RETURN INDEX
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
#include "bcs.inc"
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),NR
      CHARACTER*4 NAME
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
     1'$Id: mprnt.F,v 1.1.1.1 1997/03/28 20:49:38 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='MPRNT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK,IND
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MPRNT')
      JW(1)=0
      JW(2)=0
      IND=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
      IND=MLINK(JW,NAME,NR)
      IF(IND.EQ.0) GOTO 100
      CALL WPRNT(JW,IND)
  100 MPRNT=IND
      RETURN
      END
