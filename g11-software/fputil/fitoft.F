CDECK  ID>, FITOFT.
      SUBROUTINE FITOFT(IND)
c
c_begin_doc
c  RCS ID string
c  $Id: fitoft.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $
c
c  Purpose:
c  --------
c***********************************************************************
c Transform intermediate integer column workbank to full bank with     *
c name /nr as defined in IW(IND-3:IND-2)                               *
c Define original BOS bank format                                      *
c Input: IND:    index of where uncompressed output bank should go.    *
c        INDWO3: workbank index with input data                        *
c Output: new bank with fully expanded data                            *
c                                                                      *
c Warning: do not use MBANK, NBANK and therefore BKFRW in this routine *
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
#include "bcscom.inc"
#include "cwkfto.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IND
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
     1'$Id: fitoft.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FITOFT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER JARR,NCOLS,NCOLS2,LFMT,LFBIT,ND2,ND1,NROWS,ICOL,
     &        ICOL2,NOFF,IEMIN,NS,INDWOX,INDX,IROW,IMANT,IEXP,NR1,NR2,
     &        NR3,NPAK,NEXP
      REAL XARR,ALOG16,FLSIXT,EPS,R1,R2
      EQUIVALENCE (XARR,JARR)
      CHARACTER*4 NABANK,CHAINT
      LOGICAL FIRST
      CHARACTER*3 FMT(3)
      DIMENSION FLSIXT(-8:8)
* Initialisations:
      DATA FIRST/.TRUE./
      DATA FMT/'B32','B16','B08'/
c_end_var
c----6----------------------------------------------------------------72
      IF(FIRST)THEN
        FIRST=.FALSE.
 
*       calculate constants for decoding covariance matrix
 
        ALOG16=ALOG(16.0)
        FLSIXT(-8)=16**7
        FLSIXT(-8)=16.0*FLSIXT(-8)
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
 
*    workbank there ?
 
      IF(INDWO3.EQ.0)GOTO 1000
 
* initialize access to format information
 
      CALL FMTCUR(IW(IND-3),NCOLS,NCOLS2)
      IF(NCOLS.EQ.-1)THEN
        CALL ERRLOG(6587,'S: format definition missing:'//NABANK)
        STOP
      ENDIF
 
*     check if B32/16/08 format
 
      IF(NCOLS2.EQ.-NCOLS)THEN
        NABANK=CHAINT(IW(IND-3))
* format definition always fits in one word in this case (only 2 bits)
        CALL FMTFMT(LFMT,LFBIT)
        IF(LFBIT.NE.2.OR.LFMT.LT.1.OR.LFMT.GT.3)THEN
          WRITE(6,*)' FITOFT: illegal format:',LFMT,LFBIT
          STOP
        ENDIF
        CALL BKFMT(NABANK,FMT(LFMT))
*       go back to compressed banks: quite stupid, but unavoidable.
        ND2=IW(INDWO3)
        IF(LFMT.EQ.3) THEN
          ND1=(ND2-1)/NBYTWD+1
        ELSE IF(LFMT.EQ.2) THEN
          ND1=(ND2-1)/(NBYTWD/2)+1
        ELSE IF(LFMT.EQ.1) THEN
          ND1=ND2
        END IF
        IW(IND)=ND1
        IW(IND+ND1)=0
        IF(LFMT.EQ.3) THEN
          CALL BUN8W(IW(INDWO3+1),1,IW(IND+1),1,ND2)
        ELSE IF(LFMT.EQ.2) THEN
           CALL BUN16W(IW(INDWO3+1),1,IW(IND+1),1,ND2)
        ELSE
          CALL BUN32W(IW(INDWO3+1),1,IW(IND+1),1,ND2)
        END IF
        GOTO 1000
      ENDIF
 
      IF(NCOLS2.NE.IW(INDWO3+1))THEN
        CALL ERRLOG(7346,'S: the number of columns inconsistent with
     + format in '//NABANK)
        STOP
      ENDIF
 
      NROWS=IW(INDWO3+2)
 
* create original bank with rows and columns in normal order, merge
* mantissa and exponent back to floating point values.
 
cboy      IW(IND+1)=NCOLS
cboy      IW(IND+2)=NROWS
      IW(IND-5)=NCOLS
      IW(IND-4)=NROWS
 
      ICOL2=0
 
      DO ICOL=1,NCOLS
 
*       get compression parameters for current column
        CALL FMTNXT(NOFF,IEMIN,NS)
        ICOL2=ICOL2+1
        INDWOX=INDWO3+2+(ICOL2-1)*NROWS
cboy        INDX =IND +2-NCOLS+ICOL
        INDX=IND-NCOLS+ICOL
 
* now loop over all rows and either ...
 
        IF(NOFF.EQ.0)THEN
          IF(NS.EQ.0)THEN
 
*    ....  just copy column
 
            DO  IROW=1,NROWS
              INDX=INDX+NCOLS
              IW(INDX)=IW(INDWOX+IROW)
            ENDDO
 
          ELSE
 
*    .... or subtract other integer column
            IF(NS-ICOL.GE.0)THEN
              CALL ERRLOG(7476,'F: subtr. forward not yet implemented')
              STOP
            ENDIF
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              IW(INDX)=IW(INDWOX+IROW)+IW(INDX+NS-ICOL)
            ENDDO
 
          ENDIF
 
        ELSE IF (NOFF.GE.1)THEN
 
*         need additional column for floats...
          ICOL2=ICOL2+1
          EPS=2.**(IEMIN+NOFF-149)
 
          IF(NS.EQ.0)THEN
 
*    ..... or turn one floating point column to mantissa/expo columns
 
            DO IROW=1,NROWS
              IMANT=IW(INDWOX+IROW)
              IEXP=IW(INDWOX+IROW+NROWS)
              IF(IAND(IEXP,1).EQ.0)THEN
                IEXP=IEXP/2
              ELSE
                IEXP=IEXP/2+256
              ENDIF
              INDX=INDX+NCOLS
              IF(IEXP.EQ.0)THEN
                RW(INDX)=IMANT*EPS
              ELSE IF (IEXP.EQ.256)THEN
                RW(INDX)=-IMANT*EPS
              ELSE
                JARR=IOR(ISHFT(IMANT,NOFF),ISHFT(IEXP+IEMIN,23))
* convert IEEE value to local format
                IW(INDX)=JARR
              ENDIF
            ENDDO
 
          ELSE
 
*    .... or subtract other float column and then create mantissa/exponent
 
            IF(NS-ICOL.GE.0)THEN
              CALL ERRLOG(7477,'F: subtr. forward not yet implemented')
              STOP
            ENDIF
            DO IROW=1,NROWS
              IMANT=IW(INDWOX+IROW)
              IEXP=IW(INDWOX+IROW+NROWS)
              IF(IAND(IEXP,1).EQ.0)THEN
                IEXP=IEXP/2
              ELSE
                IEXP=IEXP/2+256
              ENDIF
              INDX=INDX+NCOLS
              IF(IEXP.EQ.0)THEN
                RW(INDX)=IMANT*EPS
              ELSE IF (IEXP.EQ.256)THEN
                RW(INDX)=-IMANT*EPS
              ELSE
                JARR=IOR(ISHFT(IMANT,NOFF),ISHFT(IEXP+IEMIN,23))
* convert IEEE value to local format
                IW(INDX)=JARR
              ENDIF
              RW(INDX)=RW(INDX)+RW(INDX+NS-ICOL)
 
            ENDDO
 
          ENDIF
 
        ELSE IF(NOFF.EQ.-1)THEN
 
*    special case to treat packed cavariance matrix elements
 
*         need two aditonal columns
          ICOL2=ICOL2+2
 
          DO IROW=1,NROWS
            INDX=INDX+NCOLS
            NR1=IW(INDWOX+IROW)
            NR2=IW(INDWOX+IROW+NROWS)
            NR3=IW(INDWOX+IROW+2*NROWS)
            IF(NR1.EQ.0.AND.NR2.EQ.0.AND.NR3.EQ.0)THEN
              RW(INDX)=0.
            ELSE
              NPAK=8388608 + NR1 + 512*NR2 + 262144*(NR3/16)
              NEXP=MOD(NR3,16)-8
              RW(INDX)=-FLOAT(NPAK)*FLSIXT(NEXP)
            ENDIF
          ENDDO
 
        ELSE IF(NOFF.EQ.-2)THEN
 
*    'F' column with full precision
 
          IF(NS.EQ.0)THEN
 
*    ....  just copy column
 
            DO  IROW=1,NROWS
              INDX=INDX+NCOLS
              JARR=IW(INDWOX+IROW)
              IW(INDX)=JARR
            ENDDO
 
          ELSE
 
*    .... or subtract other float column
            IF(NS-ICOL.GE.0)THEN
              CALL ERRLOG(7476,'F: subtr. forward not yet implemented')
              STOP
            ENDIF
            DO IROW=1,NROWS
              INDX=INDX+NCOLS
              JARR=IW(INDWOX+IROW)
              R1=XARR
              JARR=IW(INDX+NS-ICOL)
              R2=XARR
              RW(INDX)=R1+R2
            ENDDO
 
          ENDIF
 
        ELSE
 
          WRITE(6,*)' ILLegal value for NOFF:',NOFF,CHAINT(IW(IND-3))
 
        ENDIF
 
*  treat next column in unpacked bank
 
      ENDDO
 
*  update total bank length
 
      IW(IND)=INDX-IND
 
1000  CALL WDROP(IW,INDWO3)
      RETURN
      END
