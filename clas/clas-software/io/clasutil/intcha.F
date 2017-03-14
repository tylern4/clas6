CDECK  ID>, INTCHA.
      INTEGER FUNCTION INTCHA(NCHAR)
c
c_begin_doc
c  RCS ID string
c  $Id: intcha.F,v 1.1 1999/07/09 22:19:48 gilfoyle Exp $
c
c  Purpose:
c  --------
c     CONVERT ARGUMENT NCHAR OF TYPE CHARACTER*4 INTO AN INTEGER,
c     WHICH CONTAINS THE FOUR CHARACTERS OF THE ARGUMENT NCHAR
c     LEFT ADJUSTED
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
     1'$Id: intcha.F,v 1.1 1999/07/09 22:19:48 gilfoyle Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='INTCHA.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER INTG
      CHARACTER*4 NCHAR
c_end_var
c----6----------------------------------------------------------------72
      CALL UCTOH(NCHAR,INTG,4,4)
      INTCHA=INTG
C
      RETURN
      END
