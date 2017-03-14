CDECK  ID>, FSEQP.
      SUBROUTINE FSEQP(IARG1,IARG2)
c
c_begin_doc
c  RCS ID string
c  $Id: fseqp.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $
c
c  Purpose:
c  --------
c        set parameter for input
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
#include "cseqr.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IARG1,IARG2
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
     1'$Id: fseqp.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FSEQP.F')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL START
      DATA START/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(START) THEN
         START=.FALSE.
         CALL FSEQST
      END IF
      LIMSEC=IARG1
      LIMREC=IARG2
  100 RETURN
      END
