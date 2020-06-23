CDECK  ID>, BBNOT.
      SUBROUTINE BBNOT(JW,SUBPGM)
c
c_begin_doc
c  RCS ID string
c  $Id: bbnot.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $
c
c  Purpose:
c  --------
c       Print error 'ARRAY NOT INITIALIZED'
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
      INTEGER JW(*)
      CHARACTER*(*) SUBPGM
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
     1'$Id: bbnot.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBNOT.F')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*5 TARR(40)
      DATA TARR          / 'ADARG','RETCD','     ','CUNIT','PUNIT',
     1                     'PRETC','PBNKS','PDAMS','ARNM1','ARNM2',
     2                     'ARNUM','ARLEN','ARADD','INAMA','IGAPA',
     3                     'IWRKA','NDELN','NDELW','IDELN','IDELW',
     4                     'I+LNK','ILSTC','ILSTE','ILSTR','ILSTS',
     5                     'ILSTT','ILSTU','     ','IIOST','CHECK',
     6                     'SRET1','SRET2','SRET3','SRET4','SRET5',
     7                     'SRET6','SRET7','SRET8','NGACN','NGACW'/
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(LUP).NE.0) THEN
         WRITE(JW(LUP),1001) SUBPGM
         WRITE(JW(LUP),1002) TARR
         WRITE(JW(LUP),1003)
         CALL BBUFM(JW,1,100,JW(LUP))
      END IF
      CALL BABEND('BBNOT')
  100 RETURN
 1001 FORMAT('0BOS++',A5,'+  ARRAY NOT INITIALIZED - STOP'/
     1       14X,'FIRST 100 WORDS OF ARRAY ARE PRINTED BELOW'/)
 1002 FORMAT(13X,10(7X,A5))
 1003 FORMAT(20X,'HISTOGRAM OF FREE WORDS IN 10 PERCENT BINS')
      END
