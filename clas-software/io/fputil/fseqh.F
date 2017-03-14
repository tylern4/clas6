CDECK  ID>, FSEQH.
      SUBROUTINE FSEQH(NAME,NR,IRUN,IEVT)
c
c_begin_doc
c  RCS ID string
c  $Id: fseqh.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $
c
c  Purpose:
c  --------
c     define another pair (name,nr) for events
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      CHARACTER*4 NAME
      INTEGER NR,IRUN,IEVT
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
     1'$Id: fseqh.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FSEQH.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER INTCHA
c_end_var
c----6----------------------------------------------------------------72
      IF(NRE.GE.3) GOTO 100
      NRE=NRE+1
      NAMERE(NRE)=INTCHA(NAME)
      NUMMRE(NRE)=NR
      IRUNRE(NRE)=IRUN
      IEVTRE(NRE)=IEVT
  100 RETURN
      END
