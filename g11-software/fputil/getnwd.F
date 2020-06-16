CDECK  ID>, GETNWD.
      SUBROUTINE GETNWD(TEXT,WORD,IPOS)
c
c_begin_doc
c  RCS ID string
c  $Id: getnwd.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $
c
c  Purpose:
c  --------
c   GETNWD parses string  with separator blank
c   Input : TEXT
c   Output : String parsed without blank before
c   IPOS  : position of the next blank in string TEXT
c   IPOS = -1 if text= ' '
c   IPOS = 1 if no blank in text
c   IPOS = 0 if no blank after last char of text
c
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      CHARACTER*(*) TEXT,WORD
      INTEGER IPOS
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
     1'$Id: getnwd.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='GETNWD.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER L,LW,I,IP,IL,LENB
c_end_var
c----6----------------------------------------------------------------72
      L  = LENB(TEXT)
      LW = LEN(WORD)
      WORD = ' '
      IPOS = -1
      IF (TEXT. EQ.' ') RETURN
      DO 10 I =1,L
        IP = INDEX(TEXT(I:L), ' ')
        IF(IP.NE.1) GOTO 20
  10  CONTINUE
      WORD(1:LW)=TEXT(1:IL)
      IPOS = 1
      RETURN
  20  IF(IP.NE.0) THEN
        IL =I+IP-2
        WORD (1:LW) =TEXT(I:IL)
        IPOS = IL+1
      ELSE IF(IP.EQ.0) THEN
        WORD(1:LW) = TEXT(I:L)
        IPOS = 0
      ENDIF
      RETURN
      END
