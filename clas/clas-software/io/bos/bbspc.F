CDECK  ID>, BBSPC.
      SUBROUTINE BBSPC(JW,SUBPGM)
c
c_begin_doc
c  RCS ID string
c  $Id: bbspc.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $
c
c  Purpose:
c  --------
c       Print error 'INSUFFICIENT SPACE'
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
     1'$Id: bbspc.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBSPC.F')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*40  TEXT(6)
      DATA TEXT/'INSUFF.SPACE FOR NEW BANK         BANK= ',
     2          'INSUFF.SPACE TO INCR. LENGTH      BANK= ',
     3          '                                        ',
     4          'READ ERROR                              ',
     5          'READ ERROR (FORMAL ERROR)               ',
     6          'READ ERROR (INSUFFICIENT SPACE)         '/
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(LUP).GT.0) THEN
         WRITE(JW(LUP),1001) SUBPGM
         WRITE(JW(LUP),1002)
         CALL BOSBK(JW)
      END IF
      CALL BABEND('BBSPC')
  100 RETURN
 1001 FORMAT('0BOS++',A5,'+  INSUFFICIENT SPACE - STOP')
 1002 FORMAT(14X,'USER RESPONSE: CALL BNRES BEFORE INITIALIZATION',
     1  ' TO INCREASE NUMBER OF RESERVED WORDS BY 100')
      END
