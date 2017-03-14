CDECK  ID>, BOSRPT.
      SUBROUTINE BOSRPT
c
c_begin_doc
c  RCS ID string
c  $Id: bosrpt.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $
c
c  Purpose:
c  --------
c     REPORT ON OPERATION
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
     1'$Id: bosrpt.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSRPT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,J,ID,IS,IDX,NHALF,NUM1,NUM2,IOCT1(2),IOCT2(2)
      CHARACTER*4 NAM1,NAM2,CHAINT, TEXTF1*32, TEXTF2*32
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(IDNAM).EQ.0.OR.IW(LUP).EQ.0) GOTO 100
      WRITE(IW(LUP),103)
      IDX=0
      CALL WBANK(IW,IDX,IW(IDNAM),1,*100)
      CALL SORTBL(IW(IDNAM+1),IW(IDX+1),-IW(IDNAM))
      NHALF=IW(IDNAM)/2
      IS   =IW(IDNAM)-NHALF
      IF(IS.GT.10) IS=((IS+9)/10)*10
*     pointer to io-statistic
      ID   =IW(IOS)
      WRITE(IW(LUP),101)
      DO 10 I=1,IS
      IF(MOD(I,10).EQ.1) WRITE(IW(LUP),'(1X)')
      J=IW(IDX+I)
      NAM1=CHAINT(IW(IDNAM+J))
      NUM1=J
      IOCT1(1)=0
      IOCT1(2)=0
      IF(ID.NE.0) THEN
        IF(2*J.LE.IW(ID)) THEN
          IOCT1(1)=IW(ID+2*J-1)
          IOCT1(2)=IW(ID+2*J  )
        END IF
      END IF
      CALL BOSFMT(NAM1,TEXTF1)
      IF(I+IS.LE.IW(IDNAM)) THEN
        J=IW(IDX+I+IS)
        NAM2=CHAINT(IW(IDNAM+J))
        NUM2=J
        IOCT2(1)=0
        IOCT2(2)=0
        IF(ID.NE.0) THEN
          IF(2*J.LE.IW(ID)) THEN
            IOCT2(1)=IW(ID+2*J-1)
            IOCT2(2)=IW(ID+2*J  )
          END IF
        END IF
        CALL BOSFMT(NAM2,TEXTF2)
        WRITE(IW(LUP),102) NUM1,NAM1,IOCT1,TEXTF1,NUM2,NAM2,IOCT2,TEXTF2
      ELSE
        WRITE(IW(LUP),102) NUM1,NAM1,IOCT1,TEXTF1
      END IF
   10 CONTINUE
      CALL WDROP(IW,IDX)
  100 RETURN
  101 FORMAT
     +(1X,2(' pos  Name      in     out  Format................  ',12X))
  102 FORMAT(1X,2(I4,2x,A4,2I8,2X,A32,4X))
  103 FORMAT('0The table below shows all used names in alphabetical orde
     +r. The column ''pos'' gives the order,'/
     +' in which the names were made known to the system (useful in read
     +ing dumps).'/
     +          ' The numbers under ''in'' and ''out'' count the number
     +of records, in which banks of the name appeared.'/)
      END
