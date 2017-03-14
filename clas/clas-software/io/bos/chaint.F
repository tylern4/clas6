CDECK  ID>, CHAINT.
      CHARACTER*4 FUNCTION CHAINT(INTG)
c
c_begin_doc
c  RCS ID string
c  $Id: chaint.F,v 1.3 1999/07/21 14:14:48 fklein Exp $
c
c  Purpose:
c  --------
c     CONVERT ARGUMENT INTG OF TYPE INTEGER INTO A CHARACTER*4 VARIABLE.
c     THE ARGUMENT INTG CONTAINS FOUR CHARCATERS, LEFT ADJUSTED.
c     THESE HAVE TO BE STORED IN A CHARACTER VARIABLE.
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
     1'$Id: chaint.F,v 1.3 1999/07/21 14:14:48 fklein Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='CHAINT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER INTG
      CHARACTER*4 CHAIN
c_end_var
c----6----------------------------------------------------------------72
      CALL UHTOC(INTG,4,CHAIN,4)
      CHAINT=CHAIN
C
      RETURN
      END
