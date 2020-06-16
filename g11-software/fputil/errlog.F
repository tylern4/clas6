CDECK  ID>, ERRLOG.
      SUBROUTINE ERRLOG(ID,TEXT)
c
c_begin_doc
c  RCS ID string
c  $Id: errlog.F,v 1.1.1.1 1997/03/28 20:55:41 marki Exp $
c
c  Purpose:
c  --------
c   S.Boiarinov : dummy !!!!!!!!!!!!!!!!!
c Input:  ID:    error identifier
c         TEXT:  textstring connected to ID
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
      INTEGER ID
      CHARACTER*(*) TEXT
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
     1'$Id: errlog.F,v 1.1.1.1 1997/03/28 20:55:41 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='ERRLOG.F')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c----6----------------------------------------------------------------72
C
      WRITE(6,*) ' ERRLOG: ID=',ID,'    TEXT=',TEXT
C
      RETURN
      END
