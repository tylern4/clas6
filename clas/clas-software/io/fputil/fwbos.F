CDECK  ID>, FWBOS.
      SUBROUTINE FWBOS(JW,LUN,LIST,IER)
c
c_begin_doc
c  RCS ID string
c  $Id: fwbos.F,v 1.2 1997/04/12 18:03:27 freyberg Exp $
c
c  Purpose:
c  --------
c            write set of banks using F-package
c        Write out one event (=logical record) to unit LUN; banks
c        are defined in list LIST.
c        Compress banks before output if requested via a call to
c        FMTDEF. Still to be done: FFTOIT check if used only for IW
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
caf#include "bcscom.inc"
#include "bcs.inc"
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),LUN,IER
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
     1'$Id: fwbos.F,v 1.2 1997/04/12 18:03:27 freyberg Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FWBOS.F')
c
c  Local User defined variables:
c  -----------------------------
compress
      INTEGER NCOLS,NCOLS2,NDATO
      LOGICAL SUCCES
compress
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER ISPEC,IKY,ICH,IND,NUM1,NUM2,NDAT,ICOUNT,LENLS,JLS,NLS
      INTEGER MLINK,INTCHA,NAMIND,ILS,NAMA,NWNAM,IOA,NR,NCOL,NROW,ID,NCH
      CHARACTER*(*) FMTEXT*80
      CHARACTER*4   NAME, NAMKEY*8,NAMDAT*8, KEYTST
      CHARACTER*4 KEYB, CHA
      DATA KEYB/'+KYE'/
      DATA ISPEC/0/
c_end_var
c----6----------------------------------------------------------------72
      KEYB(4:4)=LIST(1:1)
*     ...
      IER=0
*     switch to unit
      CALL FWUNIT(LUN)
*     origin is 1=BOS
      CALL FWPARM('ORIGIN',1)
*
      IF(LIST.EQ.'0') GOTO 90
*     link   key-bank
      IKY=MLINK(JW,KEYB,0)
*     if key lost or 'UNKNOWN' try to create key again.
      IF(IKY.NE.0)THEN
         ICH=JW(IKY+1)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
         KEYTST=CHA
      ELSE
         KEYTST=' '
      ENDIF
      IF(IKY.EQ.0.OR.KEYTST.EQ.'UNKN') THEN
         IND=MLINK(JW,'HEAD',0)
         IF(IND.NE.0) THEN
            CALL FWBOSK(JW,LIST,'RUNEVENT',JW(IND+2),JW(IND+3),0)
         ELSE
            ISPEC=ISPEC+1
            CALL FWBOSK(JW,LIST,'UNKNOWN',ISPEC,0,0)
         END IF
         IKY=MLINK(JW,KEYB,0)
         IF(IKY.EQ.0) STOP 'FWBOS: Record key bank can not be created'
      END IF
*     get key from bank
      ICH=JW(IKY+1)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(1:4)=CHA
      ICH=JW(IKY+2)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(5:8)=CHA
      NUM1=JW(IKY+3)
      NUM2=JW(IKY+4)
      CALL FCLPA(NDAT,JW(IKY+11))
      CALL FWKEY(NAMKEY,NUM1,NUM2,NDAT)
*     init loop on banks
      ICOUNT=0
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
 
      IF(NAMI.EQ.0) THEN
         IF(ICOUNT.EQ.0) GOTO 100
         GOTO 80
      END IF
*     init loop for this name
      IF(JW(NAMI).EQ.0) GOTO 3
*
*     IO-STATISTIC FOR THIS NAME
*
      IOA=NAMI-NSYST
      IF(JW(IOS).EQ.0.OR.JW(JW(IOS)).LT.2*IOA) THEN
         CALL WBANC(JW,JW(IOS),2*IOA+10,1,*19)
      END IF
      IF(JW(NAMI).NE.0) JW(JW(IOS)+2*IOA)=JW(JW(IOS)+2*IOA)+1
*
   19 IND=NAMI+1
   20 IND=JW(IND-1)
      IF(IND.EQ.0) GOTO 3
*     interface to F-package
      ICH=JW(IND-3)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMDAT=CHA
      NR=JW(IND-2)
      NDAT=JW(IND)         ! the number of data words
compress
      NDATO=NDAT
* now a bank is specified; check if to be compressed:
      CALL FMTCUR(ICH,NCOLS,NCOLS2)
      IF(NCOLS.GT.0) THEN
* a bank to be compressed: create intermediate workbank still to be
* solved: FFTOIT works only on IW array.
        CALL FFTOIT(IND,NCOLS,NCOLS2,NROW)
* overwrite expanded bank with compressed bank - if no compression
* can be achieved then the original bank is restored
        CALL SQUEEZ(JW,IND,SUCCES)
        IF(SUCCES) THEN
* mark bank as compressed via negative column numbers
          NCOL=-NCOLS
          NDAT=JW(IND)
          FMTEXT='B32'
          NCH=3
          CALL FMTSTA(NDATO,NDAT)
          GOTO 200
        ENDIF
      ENDIF
* normal bank:
      CALL FMTSTA(NDATO,NDATO)
compress
      NCOL=JW(IND-5)       ! the number of columns
      NROW=JW(IND-4)       ! the number of rows
*     get format
caf       write(*,*)'fwbos', idfmt, nami, nsyst, idfmt+nami-nsyst
      ID=IW(IDFMT+NAMI-NSYST)
      IF(ID.EQ.0) THEN
         FMTEXT=' '
         NCH=4
      ELSE IF(IW(ID+3).NE.0) THEN
         IF(IW(ID+3).EQ.1) FMTEXT='B32'
         IF(IW(ID+3).EQ.2) FMTEXT='B16'
         IF(IW(ID+3).EQ.3) FMTEXT='B08'
         NCH=3
      ELSE
         CALL FTITOC(IW(ID+4),IW(ID)-3,FMTEXT,NCH)
      END IF
*     count words
  200 ICOUNT=ICOUNT+NHW+NDAT
*     add data header ...
      CALL FWHDR(NAMDAT,NR,NCOL,NROW,FMTEXT(1:NCH))
C     WRITE(6,111) NAMDAT,NCH,FMTEXT(1:NCH)
C  111 FORMAT(' FWBOS > ',A8,1X,I4,1X,A)
*     ... and add data
      CALL FWDAT(NDAT,JW(IND+1))
      GOTO 20
*     end-of-output
   80 CALL FWEND(IER)
      GOTO 100
*     end-of-data - write buffer
   90 CALL FWEOD
  100 JW(1)=0
      JW(2)=0
      RETURN
      END
