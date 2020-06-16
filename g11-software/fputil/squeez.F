CDECK  ID>, SQUEEZ.
      SUBROUTINE SQUEEZ(IW,IND,SUCCES)
c
c_begin_doc
c  RCS ID string
c  $Id: squeez.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $
c
c  Purpose:
c  --------
c Compress table with only integer columns. If no real compression
c is achieved then the original bank is kept.
c
c Input: IND: position of original bank to be overwritten with
c             compressed bank.
c        INDWO: workbank index for workbank containing intermediate
c              integer column bank.
c Output: compressed bank with same nr/number as original bank.
c         SUCCES: logical variable. TRUE if compression was achieved.
c
c data structure of compressed bank on file:
c ------------------------------------------
c    NCOLS in original bank  taken from FPACK Data block header
c                            negative values signal a compressed
c                            data block
c    NROWS in original bank  taken from FPACK Data block header
c
c 1         LFBIT = number of bits needed for compressed format
c 2..1+LF   LFMT(1..LF)  = compressed format definiton
c 2+LF...
c
c    Further data is organized as a bit-stream, packed without holes into
c    32 bit words. NCOLS2 is derived from NCOLS and LFMT.
c
c     NCOLS2*{
c      1. column header: Bit 0: sorted yes/no
c                        Bit 1: signed values yes/no
c                        Bit 2: not yet used
c                        Bit 3..7: number of bits used for each word -1
c                                  (can be NBIT= 1..32)
c
c      2.  then a number of bitfields of width NBIT, read one by one
c          until the number of decoded words is equal NROWS.
c          Meaning of these words:
c           n * { NWORDS = the number of normal words following;
c                 NWORDS words itself;
c                 NREP   = the number of repetitions -3 of following word;
c                 word to be replicated ;
c               }
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
#include "cwkfto.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IW(*),IND
      LOGICAL SUCCES
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
     1'$Id: squeez.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='SQUEEZ.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      CHARACTER*4 CHAINT,NABANK
      INTEGER MASKS,I,IEXP,NMASKS,NCOLS,NCOLS2,NROWS,LF,LFBIT,INDWR,
     &        INDW,JBIT,INDRD,ICOL,IROW,INDSAV,ISAVE,IEND,INDWR0,
     &        INDCOU,IWORD,LENRUN,LMIN,LMAX,INDX,MAXX,NBITC,
     &        ICHEAD,NBIT,IOUT,JBITN,LOLD,LNEW,MBANK
 
*     bit masks with 0..32 consecutive bits set (MASKS(0)=0)
      DIMENSION MASKS(0:32),NMASKS(0:32)
*     word with bit at  position 0..31 is set (IEXP(32)=0)
      DIMENSION IEXP(0:32)
      LOGICAL SORTED,FIRST
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
        INDWO2=0
      ENDIF
 
      SUCCES=.FALSE.
      IF(INDWO.EQ.0)RETURN
      NABANK=CHAINT(IW(IND-3))
 
*     get format definition
      CALL FMTCUR(IW(IND-3),NCOLS,NCOLS2)
*     turn sign of NCOLS2 for B08/16/32 banks
      IF(NCOLS2.LT.0)NCOLS2=-NCOLS2
      IF(NCOLS.LT.0)THEN
        WRITe(6,*)' SQUEEZ: format missing:',NABANK
        STOP
      ENDIF
 
      NROWS=IW(INDWO+2)
      IF(NROWS.LE.0)GOTO 2000
 
      IF(NCOLS2.NE.IW(INDWO+1))THEN
        WRITE(6,*)' SQUEEZ: inconsistent columns number:',NABANK,
     +  NCOLS2,IW(INDWO+1)
        STOP
      ENDIF
 
* define a further workbank with sufficient space to keep
* the data of the bank after RLE and subtraction of row neighbours
* and finally the data after bit compression
 
* worst case treatment:leave one word free for each column for later bitmoving
* this leaves another NCOLS words free for RLE headers of columns
 
*   some estimate of format header length
 
      LF=10
      CALL WBANK(IW,INDWO2,1+LF+NROWS*NCOLS2+2*NCOLS2,1,*9999)
*   add format information as bank header
      CALL FMTFMT(IW(INDWO2+2),IW(INDWO2+1))
 
      LFBIT=IW(INDWO2+1)
      LF=(LFBIT-1)/32+1
*  reserve additional space if required by format
      IF(LF.GT.10)THEN
        CALL WBANK(IW,INDWO2,1+LF+NROWS*NCOLS2+2*NCOLS2,1,*9999)
      ENDIF
*   here we start writing the words before bit compression
      INDWR=INDWO2+1+LF+NCOLS2
*   and here the bitcompressed data
*   (this initializes the PUTBIT sequences.)
      INDW=INDWO2+1+LF
      JBIT=32
* index of last word read from input bank
      INDRD=INDWO+2
 
      DO ICOL=1,NCOLS2
 
* check if sorted, currently only ascending values are checked
        SORTED=.FALSE.
        DO IROW=1,NROWS-1
          IF(IW(INDRD+IROW+1).LT.IW(INDRD+IROW))GOTO 23
        ENDDO
        SORTED=.TRUE.
23      CONTINUE
 
*   overwrite last-1 word in input bank with zero in order to allow
*   subtration of previous element for sorted case
*   save value which is overwritten for later restoring.
 
        IF(SORTED)THEN
          INDSAV=INDRD
          ISAVE=IW(INDSAV)
          IW(INDRD)=0
        ENDIF
 
          IEND=INDRD+NROWS
 
*  start always with normal sequence:
 
* remember position of first word for this column in output bank - leave
* word free for column header
 
          INDWR0=INDWR+1
          INDWR=INDWR+1
 
* index of current counter in outbank bank (either normal or repetition counter)
          INDCOU=INDWR
          IW(INDCOU)=0
 
*    loop over input words until RLE starts
 
50        INDRD=INDRD+1
*         all rows done ?
          IF(INDRD.GT.IEND)GOTO 200
          IF(SORTED)THEN
            IWORD=IW(INDRD)-IW(INDRD-1)
          ELSE
            IWORD=IW(INDRD)
          ENDIF
          IW(INDCOU)=IW(INDCOU)+1
          INDWR=INDWR+1
*         copy word to output bank
          IW(INDWR)=IWORD
*         update count of equal consecutive numbers
          IF(IW(INDCOU).EQ.1)THEN
            LENRUN=1
          ELSE
            IF(IWORD.EQ.IW(INDWR-1))THEN
              LENRUN=LENRUN+1
            ELSE
              LENRUN=1
            ENDIF
          ENDIF
*         long enough for RLE ?
          IF(LENRUN.LT.3)GOTO 50
 
*  here we start a RLE coded sequence...
 
*         correct counter of normal sequence
          IW(INDCOU)=IW(INDCOU)-3
*         position of new counter
          INDWR=INDWR-2
          INDCOU=INDWR
*         since repetition counter is always NREP>=3 we store NREP-3 (=0 here)
          IW(INDCOU)=0
*         skip next word which contains already proper contents
          INDWR=INDWR+1
 
*  now we loop over input words until RLE finished
 
60        INDRD=INDRD+1
          IF(INDRD.GT.IEND)GOTO 200
          IF(SORTED)THEN
            IWORD=IW(INDRD)-IW(INDRD-1)
          ELSE
            IWORD=IW(INDRD)
          ENDIF
*         if still same word then increment counter
          IF(IWORD.EQ.IW(INDWR))THEN
            IW(INDCOU)=IW(INDCOU)+1
            GOTO 60
          ENDIF
 
*  prepare next normal sequence
          INDCOU=INDWR+1
          IW(INDCOU)=1
          LENRUN=1
          INDWR=INDWR+2
          IW(INDWR)=IWORD
 
          GOTO 50
 
200     CONTINUE
 
        INDRD=INDRD-1
 
*  restore element overwritten in input bank for sorted banks
 
        IF(SORTED)IW(INDSAV)=ISAVE
 
*      now we loop over all words in column of outputbank and
*      determine min-max values
 
        LMIN=IW(INDWR0)
        LMAX=LMIN
        DO INDX=INDWR0+1,INDWR
          IF(IW(INDX).LT.LMIN)THEN
            LMIN=IW(INDX)
          ELSE IF(IW(INDX).GT.LMAX)THEN
            LMAX=IW(INDX)
          ENDIF
        ENDDO
 
*       number of bits needed:
        MAXX=MAX(ABS(LMIN)-1,ABS(LMAX))
        NBITC=0
80      NBITC=NBITC+1
        MAXX=ISHFT(MAXX,-1)
        IF(MAXX.GT.0)GOTO 80
        IF(LMIN.LT.0)NBITC=NBITC+1
 
        IF(NBITC.GT.32)THEN
          WRITE(6,*)' NBITC> 32 !!!!!!!!!!!!'
          STOP
        ENDIF
 
*   write column header
*   column header: Bit 0: sorted yes/no
*                  Bit 1: signed values yes/no
*                  Bit 2: not yet used
*                  Bit 3..7: number of bits used for each word -1
*                                  (can be NBIT= 1..32)
*
 
        ICHEAD=8*(NBITC-1)
        IF(SORTED)   ICHEAD=ICHEAD+1
        IF(LMIN.LT.0)ICHEAD=ICHEAD+2
 
*     now loop over all words in output bank column and merge
*     bitfields (in place replacement)
 
*       first pack column header
        NBIT=8
        IOUT=ICHEAD
*     put next NBIT least significant bits from IOUT into IW(INDW)
*     JBIT counts the number of bits already used in this word.
*     If necessary INDW is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        INDW=INDW+1
        IW(INDW)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
 
*       and then the column itself
 
        NBIT=NBITC
        IF(LMIN.GE.0)THEN
          DO I=INDWR0,INDWR
            IOUT=IW(I)
*     put next NBIT least significant bits from IOUT into IW(INDW)
*     JBIT counts the number of bits already used in this word.
*     If necessary INDW is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        INDW=INDW+1
        IW(INDW)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
          ENDDO
        ELSE
          DO I=INDWR0,INDWR
            IOUT=IW(I)
* as PUTBIT but for signed numbers, 2 complements representation
      IF(IOUT.LT.0) IOUT=IAND(IOUT,MASKS(NBIT))
*     put next NBIT least significant bits from IOUT into IW(INDW)
*     JBIT counts the number of bits already used in this word.
*     If necessary INDW is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) IW(INDW)=IOR(IW(INDW),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        INDW=INDW+1
        IW(INDW)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
          ENDDO
        ENDIF
 
*  next column
      ENDDO
 
*  copy to final bank only if shorter
 
      LOLD=IW(IND)
      LNEW=INDW-INDWO2
      SUCCES=LNEW.LT.LOLD-4
      IF(SUCCES)THEN
*       correct to final bank length, format now B32
        IW(INDWO2)=LNEW
*       copy workbank to named bank, but avoid BKFRW and BKFMT calls
        IW(1)=1
        IND= MBANK(IW,NABANK,IW(IND-2),LNEW,1)
        IF(IND.EQ.0) GOTO 9999
        IW(IND+LNEW)=0
        CALL BUN32W(IW(INDWO2+1),1,IW(IND+1),1,LNEW)
      ENDIF
 
      CALL WDROP(IW,INDWO2)
 
2000  CALL WDROP(IW,INDWO)
      RETURN
 
9999  WRITE(6,*)' SQUEEZ: Workbank problems...'
      STOP
      END
