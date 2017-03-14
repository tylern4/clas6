CDECK  ID>, UNSQEZ.
      SUBROUTINE UNSQEZ(IW,IND,NCOLS,NROWS,*)
c
c_begin_doc
c  RCS ID string
c  $Id: unsqez.F,v 1.1.1.1 1997/03/28 20:55:49 marki Exp $
c
c  Purpose:
c  --------
c***********************************************************************
c unpack bank previously packed with SQUEEZ                            *
c Input:                                                               *
c        IW:  BOS array                                                *
c        IND: bank index of bank with compressed data (but same bank   *
c             name and number as original bank                         *
c        NCOLS: number of columns in uncompressed bank.                *
c        NROWS: number of rows in uncompressed bank                    *
c Output: uncompressed bank as workbank with index INDWO3              *
c***********************************************************************
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
#include "cwkfto.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IW(*),IND,NCOLS,NROWS
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
     1'$Id: unsqez.F,v 1.1.1.1 1997/03/28 20:55:49 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='UNSQEZ.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER MASKS,I,IEXP,NMASKS,LFBIT,LF,NCOLS2,INDOUT,INDW,JWORD,
     &        JBIT,ICOL,IROWO,NBIT,IOUT,IMETH,NWORD,IIN,NREP
*     bit masks with 0..32 consecutive bits set (MASKS(0)=0)
      DIMENSION MASKS(0:32),NMASKS(0:32)
*     word with bit at  position 0..31 is set (IEXP(32)=0)
      DIMENSION IEXP(0:32)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(FIRST)THEN
        FIRST=.FALSE.
        MASKS(0)=0
        DO I=0,31
          IEXP(I)=ISHFT(1,I)
          MASKS(I+1)=IOR(MASKS(I),IEXP(I))
        ENDDO
        IEXP(32)=0
        DO I=0,32
          NMASKS(I)=NOT(MASKS(I))
        ENDDO
      ENDIF
 
* extract format definition and update it
 
      LFBIT=IW(IND+1)
      LF=(LFBIT-1)/32+1
      CALL FMTDEG(IW(IND-3),NCOLS,IW(IND+2),LFBIT)
 
*    retrieve unpacked format information
      CALL FMTCUR(IW(IND-3),NCOLS,NCOLS2)
      IF(NCOLS2.LT.0)NCOLS2=-NCOLS2
 
*   create new bank for decompressed data (all output col=Integer)
 
      INDWO3=0
      CALL WBANK(IW,INDWO3,2+NROWS*NCOLS2,1,*9999)
      IW(INDWO3+1)=NCOLS2
      IW(INDWO3+2)=NROWS
 
*    initialize putwrd sequence, INDOUT is always pointing to
*    last word written
 
      INDOUT=INDWO3+2
 
*   initialize GETBIT "function" which delivers sequentially a
*   bit field of given width.
 
* INDW: contains pointer to current 32 bit word in IW.
      INDW=IND+2+LF
      JWORD=IW(INDW)
 
* number of bits already read
      JBIT= 0
 
*   loop over columns: --------------------------------------
 
      DO 1 ICOL=1,NCOLS2
 
        IROWO=0
 
*     get column header
 
        NBIT=8
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
 
        IMETH=IAND(IOUT,7)
        NBIT=IOUT/8+1
 
*  IMETH decides how to decode the rows
 
        GOTO (10,11,12,13),IMETH+1
        WRITE(6,*)' UNSQEZ: You should never come here...'
        STOP
 
*
*     case : data is not sorted and unsigned
* ------------------------------------------
 
10      CONTINUE
 
* get normal words
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        NWORD=IOUT
*    just copy normal words
        DO I=1,NWORD
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          IIN=IOUT
* add word in IIN to next free word in output bank.
      INDOUT=INDOUT+1
      IW(INDOUT)=IIN
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
 
* get RLE sequence
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        NREP=IOUT+3
*    just copy normal words
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        IIN=IOUT
        DO I=1,NREP
* add word in IIN to next free word in output bank.
      INDOUT=INDOUT+1
      IW(INDOUT)=IIN
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
*     not yet ...
        GOTO 10
 
*
*     case : data is sorted and unsigned
* --------------------------------------
 
11      CONTINUE
 
* get normal words
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        NWORD=IOUT
*    just copy normal words
        DO I=1,NWORD
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
          IIN=IOUT
* add word in IIN to next free word in output bank and add
* contents of previous row, since we have the "sorted" case.
* still to be done: decreasing/increasing; assume here
* case increasing .
      INDOUT=INDOUT+1
      IF(IROWO.GT.0)THEN
        IW(INDOUT)=IIN+IW(INDOUT-1)
      ELSE
        IW(INDOUT)=IIN
      ENDIF
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
 
* get RLE sequence
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        NREP=IOUT+3
*    just copy normal words
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
        IIN=IOUT
        DO I=1,NREP
* add word in IIN to next free word in output bank and add
* contents of previous row, since we have the "sorted" case.
* still to be done: decreasing/increasing; assume here
* case increasing .
      INDOUT=INDOUT+1
      IF(IROWO.GT.0)THEN
        IW(INDOUT)=IIN+IW(INDOUT-1)
      ELSE
        IW(INDOUT)=IIN
      ENDIF
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
*     not yet ...
        GOTO 11
 
 
*     case : data is not sorted and signed
* ----------------------------------------
 
12      CONTINUE
 
* get normal words
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        NWORD=IOUT
*    just copy normal words
        DO I=1,NWORD
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
          IIN=IOUT
* add word in IIN to next free word in output bank.
      INDOUT=INDOUT+1
      IW(INDOUT)=IIN
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
 
* get RLE sequence
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        NREP=IOUT+3
*    just copy normal words
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        IIN=IOUT
        DO I=1,NREP
* add word in IIN to next free word in output bank.
      INDOUT=INDOUT+1
      IW(INDOUT)=IIN
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
*     not yet ...
        GOTO 12
 
*
*     case : data is sorted and signed
* ------------------------------------
 
13      CONTINUE
 
* get normal words
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        NWORD=IOUT
*    just copy normal words
        DO I=1,NWORD
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
          IIN=IOUT
* add word in IIN to next free word in output bank and add
* contents of previous row, since we have the "sorted" case.
* still to be done: decreasing/increasing; assume here
* case increasing .
      INDOUT=INDOUT+1
      IF(IROWO.GT.0)THEN
        IW(INDOUT)=IIN+IW(INDOUT-1)
      ELSE
        IW(INDOUT)=IIN
      ENDIF
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
 
* get RLE sequence
 
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        NREP=IOUT+3
*    just copy normal words
*     consecutive bit reader: reads next NBIT bits and
*     outputs them in IOUT. Pointer INDW and its contents JWORD
*     will be updated if necessary. NBIT must <=32.
*     JBIT is the number of bits already used in current word
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
        JWORD=IW(INDW)
        IF(NBIT-JBIT.LT.32)
     +    IOUT=IOR(ISHFT(IAND(JWORD,MASKS(JBIT)),NBIT-JBIT),IOUT)
        IF(JBIT.EQ.32)THEN
          JWORD=0
        ELSE
          JWORD=ISHFT(JWORD,-JBIT)
        ENDIF
      ENDIF
*    same as GETBIT for signed values, 2 complements representation
      IF(IAND(IOUT,IEXP(NBIT-1)).NE.0) IOUT=IOR(NMASKS(NBIT),IOUT)
        IIN=IOUT
        DO I=1,NREP
* add word in IIN to next free word in output bank and add
* contents of previous row, since we have the "sorted" case.
* still to be done: decreasing/increasing; assume here
* case increasing .
      INDOUT=INDOUT+1
      IF(IROWO.GT.0)THEN
        IW(INDOUT)=IIN+IW(INDOUT-1)
      ELSE
        IW(INDOUT)=IIN
      ENDIF
      IROWO=IROWO+1
        ENDDO
*    all rows done ?
        IF(IROWO.GE.NROWS)GOTO 1
*     not yet ...
        GOTO 13
 
*     next column
1     CONTINUE
 
1000  RETURN
 
9999  WRITE(6,*)' UNSQEZ: no space for workbank:',NROWS,NCOLS2
      RETURN 1
 
      END
