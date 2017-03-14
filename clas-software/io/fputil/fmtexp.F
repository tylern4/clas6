CDECK  ID>, FMTEXP.
      SUBROUTINE FMTEXP(LFMT,LFBIT,IFMT,NFMT)
c
c_begin_doc
c  RCS ID string
c  $Id: fmtexp.F,v 1.1.1.1 1997/03/28 20:55:45 marki Exp $
c
c  Purpose:
c  --------
c Expand LFMT representation used in I/O to internal format
c representation IFMT.
c
c Input:  LFMT(1..LF),LFBIT   : compressed representation
c                               of LFBIT bits
c Output: IFMT(1..NFMT),NFMT: uncompressed internal format
c
c See FMTCMP for format description
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
      INTEGER IFMT(*),LFMT(*),LFBIT,NFMT
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
     1'$Id: fmtexp.F,v 1.1.1.1 1997/03/28 20:55:45 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FMTEXP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER NADDB,NOFFB,MASKS,I,JBIT,INDW,JWORD,NBTOT,NBIT,IOUT,KF,
     &        ICODE,N,NNN,MMM
      DIMENSION NADDB(0:3),NOFFB(0:3)
*     bit masks with 0..32 consecutive bits set (MASKS(0)=0)
      DIMENSION MASKS(0:32)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NADDB/0,2,4,16/
      DATA NOFFB/1,2,6,22/
c_end_var
c----6----------------------------------------------------------------72
      IF(FIRST)THEN
*    initialize MASK array needed for EXPBIT sequence
        FIRST=.FALSE.
        MASKS(0)=0
        DO I=0,31
          MASKS(I+1)=IOR(MASKS(I),ISHFT(1,I))
        ENDDO
      ENDIF
 
* initialize bit access sequence
      JBIT=32
      INDW=0
      JWORD=0
      NBTOT=0
 
* check first two bits first
      NBIT=2
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
      NFMT=1
      IFMT(NFMT)=IOUT
*     B08 etc format ?
      IF(IOUT.GT.0)GOTO 1000
 
*     normal format, continue with expanding information
 
10    IF(NBTOT.GE.LFBIT)GOTO 1000
      NBIT=3
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
      KF=IOUT
*     except for ')' (KF=7) a number goes with KF
      NFMT=NFMT+1
      IF(KF.LT.7)THEN
        NBIT=2
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        ICODE=IOUT
        NBIT=NADDB(ICODE)
        IF(NBIT.EQ.0)THEN
          IOUT=0
        ELSE
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        ENDIF
        N=IOUT+NOFFB(ICODE)
        IFMT(NFMT)=10*N+KF
        IF(KF.EQ.4)THEN
*   two more numbers are to come:
          NBIT=2
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          ICODE=IOUT
          NBIT=NADDB(ICODE)
          IF(NBIT.EQ.0)THEN
            IOUT=0
          ELSE
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          ENDIF
          NNN=IOUT+NOFFB(ICODE)
          NBIT=2
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          ICODE=IOUT
          NBIT=NADDB(ICODE)
          IF(NBIT.EQ.0)THEN
            IOUT=0
          ELSE
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
*     NBTOT counts the total number of bits read so far.
      NBTOT=NBTOT+NBIT
      JBIT=JBIT+NBIT
      IF(JBIT.LE.32)THEN
*       all bits still in current word
        IOUT=IAND(JWORD,MASKS(NBIT))
        IF(NBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-NBIT)
        ENDIF
      ELSE
*       get first bits from current word, get next word
*       and read remaining bits
        JBIT=JBIT-32
        IOUT=JWORD
        INDW=INDW+1
        JWORD=LFMT(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          ENDIF
          MMM=IOUT+NOFFB(ICODE)
          IFMT(NFMT)=IFMT(NFMT)+10000*NNN+1000000*MMM
        ENDIF
      ELSE
        IFMT(NFMT)=0
      ENDIF
      GOTO 10
 
1000  RETURN
      END
