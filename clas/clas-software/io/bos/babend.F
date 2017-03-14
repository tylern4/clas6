CDECK  ID>, BABEND.
      SUBROUTINE BABEND(TEXT)
c
c_begin_doc
c  RCS ID string
c  $Id: babend.F,v 1.1.1.1 1997/03/28 20:49:27 marki Exp $
c
c  Purpose:
c  --------
c       BOS termitation routine
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
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
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
     1'$Id: babend.F,v 1.1.1.1 1997/03/28 20:49:27 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BABEND.F')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(LUP).NE.0) WRITE(IW(LUP),101) TEXT
  101 FORMAT('0++ABEND++++ ',A)
*
*     HERE THE USER CAN INSERT HIS OWN ABNORMAL END ROUTINE
*     IT IS HELPFUL, IF A TRACEBACK IS PRINTED
      CALL ABEND
      RETURN
      END
