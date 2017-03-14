CDECK  ID>, WDROP.
      SUBROUTINE WDROP(JW,ID)
c
c_begin_doc
c  RCS ID string
c  $Id: wdrop.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $
c
c  Purpose:
c  --------
c     USER ENTRY
c     DROP WORK BANKS WITH INDICES ID(1)...ID(NDIM)
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
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),ID(*)
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
     1'$Id: wdrop.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='WDROP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LOCF,NDIM,LD,I,JD
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WDROP')
      NDIM=MAX(1,JW(1))
      JW(1)=0
      JW(2)=0
C
      LD=LOCF(ID(1))-JW(LJW)
      DO 10 I=1,NDIM
      IF(ID(I).EQ.0) GOTO 10
C
C        DROP WORK BANK, IF INDEX VALID
C
        IF(ID(I).GT.JW(IWK).AND.ID(I).LT.JW(NJW)
     &                     .AND.JW(ID(I)-1).EQ.LD+I) THEN
          JW( NDW)=JW( NDW)+JW(ID(I)-2)    ! the number of dropped words
          JW(IGW)=MAX(JW(IGW),ID(I)+JW(ID(I)-2)-(NHW-1)) ! index of
                                             ! highest deleted work bank
          JW(ID(I))=-JW(ID(I)-2)
          ID(I)=0
          GOTO 10
        END IF
        IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) ID(I)
 1001   FORMAT('0BOS--WDROP- INDEX ID INCORRECT - STOP'/14X,'ID=',I12)
        CALL BABEND('WDROP')
   10 CONTINUE
C
C     TRY TO REGAIN SPACE OF DROPPED BANKS
C
      JD=JW(IWK)+(NHW-1)
   20 IF(JW(JD).LT.0) THEN
        JW( IWK)=JW( IWK)-JW(JD)
        JW( NDW)=JW( NDW)+JW(JD)
        JD    =JD-JW(JD)
        GOTO 20
      END IF
      IF(JW(NDW).EQ.0) JW(IGW)=0
C exit
  100 RETURN
      END
