CDECK  ID>, BLIST.
      SUBROUTINE BLIST(JW,OPT,LIST)
c
c_begin_doc
c  RCS ID string
c  $Id: blist.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $
c
c  Purpose:
c  --------
c     OPERATION WITH LISTS
c
c     OPT = 'LO'  where L is 'C','E','R','S','T' and O is '=','+','-'
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
      CHARACTER*2 OPT
      CHARACTER*(*) LIST
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
     1'$Id: blist.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BLIST.F')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*4 NAME
      INTEGER IL,IP,ID,NL,ND,LENLS,JLS,NLS,ILS,NAMA,NWNAM,I,NLL,J
      INTEGER INTCHA,NAMIND
c_end_var
c----6----------------------------------------------------------------72
C-----------------------------------------------------------------------
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BLIST')  ! check magic word
      JW(1)=0
***      JW2  =0
C check input arguments
      IL=INDEX('CERST',OPT(1:1))                   ! return 1,2,3,4 or 5
      IP=INDEX('=+-',OPT(2:2))                     ! return 1,2 or 3
      IF(IL.EQ.0.OR.IP.EQ.0) THEN                  ! wrong arguments
        IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) OPT,LIST
 1001   FORMAT('0BOS--BLIST-  WRONG ARGUMENT - STOP'/
     &       14X,'OPT,LIST= ',A4,A)
        CALL BABEND('BLIST')
      END IF
C work bank for LIST
      IL=IL+ILT                 ! pointer on index of work bank for LIST
      IF(JW(IL).EQ.0) THEN      ! if bank doesn't exist, create it
        CALL WBANC(JW,JW(IL),0,1,*91)
        JW(JW(IL)-3)=INTCHA('+LST')
      END IF
C what option we have ?
      ID=JW(IL)                       ! index of work bank for LIST
      NL=JW(ID)                       ! the number of words in this bank
      IF(IP.EQ.1) THEN                ! create new list of names ('=')
        IP=2
        NL=0
      END IF
      ND=JW(ID-2)-NHW        ! total number of words in bank
                             ! minus the number of header words
C return NAMI = name-index for next name in LIST or zero
C------ KEEP, LISTNN ------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR NEXT NAME IN LIST
C                    OR ZERO
C
      LENLS=LEN(LIST)
      IF(LENLS.EQ.0) THEN
         JLS=0
      ELSE IF(LENLS.EQ.1) THEN
         JLS=INDEX('CERST',LIST)
         IF(JLS.NE.0) THEN
            JLS=JLS+ILT
            NLS=JW(JW(JLS))
         END IF
      ELSE
         NLS=LENLS/4
         JLS=-1
      END IF
      ILS=0
    3 IF(JLS.EQ.0) THEN
         NAMI=0
      ELSE IF(JLS.LT.0) THEN
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAME=LIST(4*ILS-3:4*ILS)
            NAMA=INTCHA(NAME)
C------ KEEP, NAMEN -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR INTEGER NAMA
C                    (ALWAYS NONZERO)
      NWNAM=IW(IDNAM)
      IW(IDNAM)=NAMA
      NAMI=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1 NAMI=IW(IDPTR+NAMI)
      IF(IW(IDNAM+NAMI).NE.IW(IDNAM)) GOTO 1
      IW(IDNAM)=NWNAM
      NAMI=NAMI+NSYST
      IF(NAMI.EQ.NSYST) NAMI=NAMIND(NAME)
C--------------------------------------------------------------------
 
         ELSE
            JLS=0
            NAMI=0
         END IF
      ELSE
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAMI=JW(JW(JLS)+ILS)
         ELSE
            JLS=0
            NAMI=0
         END IF
      END IF
C--------------------------------------------------------------------
 
      IF(NAMI.NE.0) THEN
        IF(IP.EQ.2) THEN           ! add new name to list of names ('+')
          DO I=1,NL                ! looking for such name in list
            IF(JW(I+ID).EQ.NAMI) GOTO 3
          ENDDO
          IF(NL.EQ.ND) THEN   ! if bank of names is full, create new one
            JW(ID)=NL
            ND=ND+10
            CALL WBANC(JW,JW(IL),ND,1,*91)
            ID=JW(IL)
          END IF
          NL=NL+1                 ! increment the number of words
          JW(ID+NL)=NAMI          ! put name in list
          GOTO 3
        ELSE                      ! delete name from list of names ('-')
          NLL=NL
          DO I=1,NLL              ! looking for given name in the list
            IF(JW(I+ID).EQ.NAMI) THEN  ! if exist, delete it
              NL=NL-1
              DO J=I,NL
                JW(J+ID)=JW(J+ID+1)
              ENDDO
              GOTO 3
            END IF
          ENDDO
          GOTO 3
        END IF
      END IF
      JW(ID)=NL
      JW(1)=0
      JW(2)=0
      GOTO 100
C insufficient space
   91 CALL BBSPC(JW,'BLIST')
C exit
  100 RETURN
      END
