CDECK  ID>, FFTOIT.
      SUBROUTINE FFTOIT(IND,NCOLS,NCOLS2,NROWS)
c
c_begin_doc
c  RCS ID string
c  $Id: fftoit.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $
c
c  Purpose:
c  --------
c Transform bank at index IND into bank with only integer columns.     *
c This is a preparatory step for the actual compression step in SQUEEZ.*
c The conversion is steered for each bank by a CALL to the routine     *
c FMTDEF, which has to be called before FFTOIT in order to define the  *
c format of the compressed bank.                                       *
c                                                                      *
c Input:  IND    = bank index of bank to be compressed                 *
c         NCOLS,NCOLS2 = number of columns in original and intermediate*
c                        banks; negative values mean special cases,    *
c                        (see FMTCUR for details)                      *
c Output: workbank at workbank index INDWO, containing bank with only  *
c         integer columns                                              *
c         NROWS  = the number of rows in bank to be compressed         *
c***********************************************************************
c still to be done: replace ERRLOG by something else ?
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
#include "bcscom.inc"
#include "cwkfto.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IND,NCOLS,NCOLS2,NROWS
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
     1'$Id: fftoit.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FFTOIT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER JARR,MASKMS,IHID,MASKEX,MASKSI,
     &        ICOL2,ICOL,NOFF,IEMIN,NS,INDWOX,INDX,IROW,IEMINO,IEOFF,
     &        IEOFF2,IEOFF3,MAMAX,IE,IEXP,IMANT,NR1,NR2,NR3,NEXP,NPAK
      REAL XARR,ALOG16,FLSIXT,COV
      EQUIVALENCE (XARR,JARR)
      CHARACTER*4 CHAINT
      LOGICAL FIRST
      DIMENSION FLSIXT(-14:8)
* Initialisations:
      DATA FIRST/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(FIRST)THEN
        FIRST=.FALSE.
 
*       some constants for treating IEEE floating point values
 
        MASKMS=ISHFT(255,23)
        IHID=ISHFT(1,23)
        MASKEX=IHID-1
        MASKSI=ISHFT(1,31)
 
*       calculate constants for decoding strange covariance matrix
 
        ALOG16=ALOG(16.0)
        FLSIXT(-8)=16**7
        FLSIXT(-8)=16.0*FLSIXT(-8)
        FLSIXT(-9)=16.0*FLSIXT(-8)
        FLSIXT(-10)=16.0*FLSIXT(-9)
        FLSIXT(-11)=16.0*FLSIXT(-10)
        FLSIXT(-12)=16.0*FLSIXT(-11)
        FLSIXT(-13)=16.0*FLSIXT(-12)
        FLSIXT(-14)=16.0*FLSIXT(-13)
 
        FLSIXT(-7)=16**7
        FLSIXT(-6)=16**6
        FLSIXT(-5)=16**5
        FLSIXT(-4)=16**4
        FLSIXT(-3)=16**3
        FLSIXT(-2)=16*16
        FLSIXT(-1)=16
        FLSIXT( 0)=1
        FLSIXT( 1)=1.0/FLSIXT(-1)
        FLSIXT( 2)=1.0/FLSIXT(-2)
        FLSIXT( 3)=1.0/FLSIXT(-3)
        FLSIXT( 4)=1.0/FLSIXT(-4)
        FLSIXT( 5)=1.0/FLSIXT(-5)
        FLSIXT( 6)=1.0/FLSIXT(-6)
        FLSIXT( 7)=1.0/FLSIXT(-7)
        FLSIXT( 8)=1.0/FLSIXT(-8)
 
      ENDIF
 
      IF(NCOLS2.EQ.-NCOLS)THEN
*       this is a B08/16/32 bank, just unpack
        INDWO=0
        CALL BKTOW(IW,CHAINT(IW(IND-3)),IW(IND-2),IW,INDWO,*9999)
cboy        NROWS=IW(INDWO+2)
        NROWS=IW(INDWO-4)
        GOTO 1000
      ENDIF
 
cboy      IF(NCOLS.NE.IW(IND+1))THEN
cboy      print *,' NCOLS,IW(IND+1)=',NCOLS,IW(IND+1)
      IF(NCOLS.NE.IW(IND-5))THEN
cboy      print *,' NCOLS,IW(IND-5)=',NCOLS,IW(IND-5)
        CALL ERRLOG(7345,'S: the number of columns inconsistent with
     + format in '//CHAINT(IW(IND-3)))
        RETURN
      ENDIF
 
cboy      NROWS=IW(IND+2)
      NROWS=IW(IND-4)
*      WRITE(6,*)' BANK:',CHAINT(IW(IND-3)),IW(IND-2)
 
* create new workbank with rows and columns exchanged, additonal
* columns for exponents for floating point values and specials.
 
      INDWO=0
      CALL WBANK(IW,INDWO,2+NROWS*NCOLS2,1,*9999)
      IW(INDWO+1)=NCOLS2
      IW(INDWO+2)=NROWS
      IF(NROWS.EQ.0)GOTO 1000
 
* loop over all columns and check if subtraction of other column
* needed and if transformation from float to mantissa/exponent
* presentation wanted:
 
      ICOL2=0
 
      DO ICOL=1,NCOLS
 
*       get compression parameters for current column
        CALL FMTNXT(NOFF,IEMIN,NS)
        ICOL2=ICOL2+1
        INDWOX=INDWO+2+(ICOL2-1)*NROWS
cboy        INDX =IND +2-NCOLS+ICOL
        INDX=IND-NCOLS+ICOL
 
* now loop over all rows and either ...
 
        IF(NOFF.EQ.0)THEN
          IF(NS.EQ.0)THEN
 
*    ....  just copy column
 
            DO  IROW=1,NROWS
              INDX=INDX+NCOLS
              IW(INDWOX+IROW)=IW(INDX)
            ENDDO
 
          ELSE
 
*    .... or subtract other integer column
 
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              IW(INDWOX+IROW)=IW(INDX)-IW(INDX+NS-ICOL)
            ENDDO
 
          ENDIF
 
        ELSE IF (NOFF.GE.1)THEN
 
          IEMINO=IEMIN-23+NOFF
          IEOFF=IEMIN+1
          IEOFF2=IEMIN+NOFF
          IEOFF3=NOFF-1
          MAMAX=ISHFT(1,23-NOFF)
*         need additional column for floats...
          ICOL2=ICOL2+1
 
          IF(NS.EQ.0)THEN
 
*    ..... or turn one floating point column to mantissa/expo columns
 
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              XARR=RW(INDX)
* transform to IEEE floating point
              IE=ISHFT(IAND(JARR,MASKMS),-23)
              IF(IE.LT.IEMINO)THEN
                IEXP=0
                IMANT=0
              ELSE IF(IE.LT.IEOFF)THEN
                IEXP=0
                IMANT=ISHFT(IAND(JARR,MASKEX)+IHID,-(IEOFF2-IE))
                IF(IAND(IMANT,1).EQ.1)THEN
*                 round up
                  IMANT=ISHFT(IMANT,-1)+1
                  IF(IMANT.EQ.MAMAX)THEN
                    IMANT=0
                    IEXP=IEXP+1
                  ENDIF
                ELSE
*                 round down
                  IMANT=ISHFT(IMANT,-1)
                ENDIF
              ELSE
                IEXP=IE-IEOFF+1
                IMANT=ISHFT(IAND(JARR,MASKEX),-IEOFF3)
                IF(IAND(IMANT,1).EQ.1)THEN
*                 round up
                  IMANT=ISHFT(IMANT,-1)+1
                  IF(IMANT.EQ.MAMAX)THEN
                    IMANT=0
                    IEXP=IEXP+1
                  ENDIF
                ELSE
*                 round down
                  IMANT=ISHFT(IMANT,-1)
                ENDIF
              ENDIF
*             add sign:
              IEXP=2*IEXP
cboy ?????  if defined(SUN)||defined(SGI)||defined(MIPS)
cboy ?????               IF(XARR.LT.0.)IEXP=IEXP+1
cboy ?????  else
              IF(IAND(JARR,MASKSI).NE.0)IEXP=IEXP+1
cboy ?????  endif
*             store values
              IW(INDWOX+IROW)=IMANT
              IW(INDWOX+IROW+NROWS)=IEXP
             ENDDO
 
          ELSE
 
*    .... or subtract other float column and then create mantissa/exponent
 
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              XARR=RW(INDX)-RW(INDX+NS-ICOL)
* transform to IEEE floating point
              IE=ISHFT(IAND(JARR,MASKMS),-23)
              IF(IE.LT.IEMINO)THEN
                IEXP=0
                IMANT=0
              ELSE IF(IE.LT.IEOFF)THEN
                IEXP=0
                IMANT=ISHFT(IAND(JARR,MASKEX)+IHID,-(IEOFF2-IE))
                IF(IAND(IMANT,1).EQ.1)THEN
*                 round up
                  IMANT=ISHFT(IMANT,-1)+1
                  IF(IMANT.EQ.MAMAX)THEN
                    IMANT=0
                    IEXP=IEXP+1
                  ENDIF
                ELSE
*                 round down
                  IMANT=ISHFT(IMANT,-1)
                ENDIF
              ELSE
                IEXP=IE-IEOFF+1
                IMANT=ISHFT(IAND(JARR,MASKEX),-IEOFF3)
                IF(IAND(IMANT,1).EQ.1)THEN
*                 round up
                  IMANT=ISHFT(IMANT,-1)+1
                  IF(IMANT.EQ.MAMAX)THEN
                    IMANT=0
                    IEXP=IEXP+1
                  ENDIF
                ELSE
*                 round down
                  IMANT=ISHFT(IMANT,-1)
                ENDIF
              ENDIF
*             add sign:
              IEXP=2*IEXP
cboy ?????  if defined(SUN)||defined(SGI)||defined(MIPS)
cboy ?????               IF(XARR.LT.0.)IEXP=IEXP+1
cboy ?????  else
              IF(IAND(JARR,MASKSI).NE.0)IEXP=IEXP+1
cboy ?????  endif
*             store values
              IW(INDWOX+IROW)=IMANT
              IW(INDWOX+IROW+NROWS)=IEXP
            ENDDO
 
          ENDIF
 
        ELSE IF(NOFF.EQ.-1)THEN
 
*    special case to treat packed cavariance matrix elements
 
*         need two additional columns
          ICOL2=ICOL2+2
 
          DO IROW=1,NROWS
            INDX=INDX+NCOLS
            COV=RW(INDX)
            IF(COV.EQ.0.)THEN
              NR1=0
              NR2=0
              NR3=0
            ELSE
              NEXP=ALOG(-COV)/ALOG16
*          off by one for numbers near 16**N
              IF(FLSIXT(-NEXP).GT.-COV)NEXP=NEXP-1
              NEXP=5-NEXP
              NPAK=NINT(-COV*FLSIXT(-NEXP))
              NPAK=NPAK-8388608
              NR1=MOD(NPAK,512)
              NR2=MOD(NPAK/512,512)
              NR3=(NPAK/262144)*16+NEXP+8
* still to be simplified...
              IF(-NEXP.GT.8.OR.NR1.LT.0.OR.NR2.LT.0.OR.NR3.LT.0
     +  .OR. NR1.GT.511.OR.NR2.GT.511.OR.NR3.GT.511)THEN
                WRITE(6,*)' Y Format:',NEXP,NR1,NR2,NR3,IROW,ICOL,
     +          CHAINT(IW(IND-3)),COV,ALOG(-COV)/ALOG16,NPAK,
     +          -COV*FLSIXT(-NEXP),FLSIXT(-NEXP),-COV
                STOP
              ENDIF
            ENDIF
            IW(INDWOX+IROW)=NR1
            IW(INDWOX+IROW+NROWS)=NR2
            IW(INDWOX+IROW+2*NROWS)=NR3
          ENDDO
 
        ELSE IF(NOFF.EQ.-2)THEN
 
*    turn all 'F' to IEEE numbers keeping full precision
 
          IF(NS.EQ.0)THEN
 
            DO  IROW=1,NROWS
              INDX=INDX+NCOLS
              JARR=IW(INDX)
              IW(INDWOX+IROW)=JARR
            ENDDO
 
          ELSE
 
*    .... or subtract other F column
 
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              XARR=RW(INDX)-RW(INDX+NS-ICOL)
              IW(INDWOX+IROW)=JARR
            ENDDO
 
          ENDIF
 
        ELSE
 
          WRITE(6,*)' Illegal value for NOFF:',NOFF,CHAINT(IW(IND-3))
          STOP
 
        ENDIF
 
*  treat next column in unpacked bank
 
      ENDDO
 
      GOTO 1000
 
9999  WRITE(6,*)' FFTOIT: insufficient space for workbank'
      STOP
 
1000  RETURN
 
      END
