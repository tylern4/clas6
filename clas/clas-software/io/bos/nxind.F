CDECK  ID>, NXIND.
      INTEGER FUNCTION NXIND(IND)
c
c_begin_doc
c  RCS ID string
c  $Id: nxind.F,v 1.1.1.1 1997/03/28 20:49:41 marki Exp $
c
c  Purpose:
c  --------
c     DETERMINE INDEX OF BANK BEHIND BANK AT INDEX IND
c        (=NEXT BANK IN BOS-ARRAY)
c     IF IND=0 INDEX OF FIRST BANK IN STORAGE IS RETURNED
c     IF IND<0 INDEX OF FIRST BANK OF LAST READ OPERATION IS RETURNED
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
      INTEGER IND
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
     1'$Id: nxind.F,v 1.1.1.1 1997/03/28 20:49:41 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='NXIND.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JND
      INTEGER JW(1000)
      EQUIVALENCE (JW(1),IW(1))
c_end_var
c----6----------------------------------------------------------------72
      JND=IND
      IF(JND.LE.0) THEN
*        INDEX OF FIRST BANK IN ARRAY
         JND=JW(INM)+(NHW-1)
      ELSE
         IF(JW(JND).GT.0) THEN
            JND=JND+JW(JND)+NHW
         ELSE
            JND=JND-JW(JND)
         END IF
      END IF
   10 IF(JND.GT.JW(IGP)) THEN
         JND=0
      ELSE
         IF(JW(JND).LT.0) THEN
            JND=JND-JW(JND)
            GOTO 10
         END IF
      END IF
  100 NXIND=JND
      RETURN
      END
