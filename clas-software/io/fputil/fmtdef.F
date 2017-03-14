CDECK  ID>, FMTDEF.
      SUBROUTINE FMTDEF(NABANK,NCOLS,FMT)
c
c_begin_doc
c  RCS ID string
c  $Id: fmtdef.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $
c
c  Purpose:
c  --------
c Define format of tabular bank for compression
c ---------------------------------------------
c Input: NABANK: name of bank
c        NCOLS:  number of columns in bank.
c        FMT:    Format string (without table miniheader format '2I')
c
c The information is stored internally in a workbank and can be
c retrieved by calls to the routines (entry points):
c
c  FMTCUR: retrieve global infomation for given bank
c          define the "current" bank
c  FMTNXT: get compression parameters for next column in current bank
c  FMTFMT: get compressed format information for current bank
c  FMTSTA: collect statistics about bank sizes before/after compression
c  FMTPRT: print statistics
c  FMTDDL: print precision parameters + DDL column names
c
c Internally used utility routines:
c
c   NXTFMT: utility routine to return next format subexpression
c   FMTDEC: Turn string representation into internal IFMT representation
c   FMTENC: Turn internal IFMT representation to string representation
c   FMTCMP: compress internal IFMT representation to compressed representation
c           for storage of format in bank
c   FMTEXP: expand compressed format representation to IFMT representation
c
c The format string does not define the format of the header, which
c is assumed to be always 2I. Only tabular banks can be treated.
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER NCOLS
      CHARACTER*4 NABANK
*     stores string representation of format
      CHARACTER*(*) FMT
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
     1'$Id: fmtdef.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBKMAX,NFMMAX,NPAR,INWRK,NBANK,NTOTO,NTOT,NCOUNT,
     &        IDEF,NBA,NABA,LLFMT,LLFBIT,LFBIT,I,LFMT,IFMT,NFMT,LF,
     &        NUSED,IBANK,NAMBNK,IND,NSHIFT,IND1,NNEW,IOFF,NCOLS2,INDYY,
     &        N,K,NCIN,NWORD,KF,NNN,NREP,MMM,NF,II,NCSUB,LCOLS,LCOLS2,
     &        LBANK,INXX,NDATO,NDAT,NTOTSU,NTOTSO,NCOUSU,NC,ICOL,NHELP,
     &        MHELP,NOFF,IEMIN,NS,LFMTS,LFBITS,INX,INTCHA
      REAL RERR,AERR
      CHARACTER*4 CHAINT
cboy      CHARACTER*4 NAM,FRMDDL
cboy      CHARACTER*8 CONAME,FORM
      CHARACTER*4 NAM
      CHARACTER*8 FORM
      CHARACTER*3 FMTB16(3)
      PARAMETER (NBKMAX=200)
      DIMENSION IND1(NBKMAX),NAMBNK(NBKMAX)
      DIMENSION NTOTO(0:NBKMAX),NTOT(0:NBKMAX),NCOUNT(0:NBKMAX)
      PARAMETER (NFMMAX=200)
      DIMENSION IFMT(NFMMAX),LFMT(NFMMAX),LFMTS(*),LLFMT(*)
*     number of compression parameters stored per column
      PARAMETER (NPAR=3)
      COMMON/CXYWSD/INWRK
      DATA NBANK/0/
      DATA NTOTO/0,NBKMAX*0/
      DATA NTOT /0,NBKMAX*0/
      DATA NCOUNT/0,NBKMAX*0/
      DATA FMTB16/'B32','B16','B08'/
c
c The workbank at INWRK stores the format definition of all banks defined
c via FMTDEF for fast access to the parameters. A maximum of NBKMAX
c different banks is allowed.
c
c Structure of work bank:
c
c IND1(IBANK) ->LFBIT       : number of bits used in LFMT, LF=(LFBIT-1)/32+1
c               LFMT(1...LF): compressed
c               NCOLS       : number of columns in original bank
c               NCOLS2      : number of columns of intermediate bank
c                             (=-NCOLS for B08 etc banks)
c               remaining words are only used for "normal" formats:
c               NCOLS*(column information: NPAR words per column)
c_end_var
c----6----------------------------------------------------------------72
      IDEF=1
      NBA=INTCHA(NABANK)
      GOTO 10
 
      ENTRY FMTDEG(NABA,NCOLS,LLFMT,LLFBIT)
************************************************************************
* Entry point if compressed format representation is known
* Otherwise identical to FMTDEF
***********************************************************************
      IDEF=0
      NBA=NABA
      LFBIT=LLFBIT
      DO I=1,(LFBIT-1)/32+1
        LFMT(I)=LLFMT(I)
      ENDDO
 
10    CONTINUE
 
      IF(IDEF.EQ.1)THEN
        CALL FMTDEC(FMT,IFMT,NFMT)
        IF(NFMT.GT.NFMMAX)THEN
          CALL ERRLOG(8787,'F: Format definition too long')
          STOP
        ENDIF
*        WRITE(6,*)' Internal format:',NABANK,':',FMT
*        WRITE(6,'(1X,2I10)')(I,IFMT(I),i=1,NFMT)
        CALL FMTCMP(IFMT,NFMT,LFMT,LFBIT)
      LF=(LFBIT-1)/32+1
*       WRITe(6,*)' compressed to:',LFBIT,(LFMT(I),I=1,LF)
      ENDIF
 
*   number of full 32 bit words needed for format
      LF=(LFBIT-1)/32+1
 
*   create workbank with initial size of 100 words
 
      IF(NBANK.EQ.0)THEN
        NUSED=0
        INWRK=0
        CALL WBANK(IW,INWRK,100,1,*9999)
      ENDIF
 
*     define bank index, update tables of known banks
*     could still to be improved via TLU routines
 
      DO IBANK=1,NBANK
        IF(NBA.EQ.NAMBNK(IBANK))GOTO 20
      ENDDO
 
15    NBANK=NBANK+1
      IF(NBANK.GT.NBKMAX)THEN
        CALL ERRLOG(5439,'F: too many banks for compression')
        STOP
      ENDIF
      IBANK=NBANK
      NAMBNK(IBANK)=NBA
      GOTO 35
 
20    CONTINUE
*     check if format has changed
      IND=IND1(IBANK)+INWRK
      IF(NCOLS.NE.IW(IND+LF+1))GOTO 30
      IF(LFBIT.EQ.IW(IND))THEN
        DO I=1,LF
          IF(LFMT(I).NE.IW(IND+I))GOTO 30
        ENDDO
*       format unchanged, we are ready
        GOTO 1000
      ENDIF
 
30    CONTINUE
*     case that format for bank has changed:reorganize
*     workbank
 
*     move all higher bank descriptions down by one
 
      IF(IBANK.LT.NBANK)THEN
        NSHIFT=IND1(IBANK+1)-IND1(IBANK)
        DO I=IND1(IBANK+1),NUSED
          IW(INWRK+I-NSHIFT)=IW(INWRK+I)
        ENDDO
        DO I=IBANK,NBANK-1
          NAMBNK(I)=NAMBNK(I+1)
          IND1(I)=IND1(I+1)-NSHIFT
        ENDDO
      ENDIF
      IF(IBANK.EQ.NBANK)THEN
        NUSED=IND1(NBANK)-1
      ELSE
        NUSED=NUSED-NSHIFT
      ENDIF
      NBANK=NBANK-1
      GOTO 15
 
*
35    CONTINUE
*     add new bank format information to workbank:
 
      LF=(LFBIT-1)/32+1
      IF(IDEF.EQ.0)CALL FMTEXP(LFMT,LFBIT,IFMT,NFMT)
*     define start of information
      IND1(IBANK)=NUSED+1
      IF(LFBIT.EQ.2)THEN
        NNEW=1+LF+2
      ELSE
        NNEW=1+LF+2+NPAR*NCOLS
      ENDIF
      NUSED=NUSED+NNEW
*     increase workbank if necessary
      IF(NUSED.GT.IW(INWRK))THEN
        CALL WBANK(IW,INWRK,NUSED+100,1,*9999)
      ENDIF
 
*     store LFBIT,LFMT and NCOLS in workbank
      IOFF=INWRK+NUSED-NNEW+1
      IW(IOFF)=LFBIT
      DO I=1,LF
        IW(IOFF+I)=LFMT(I)
      ENDDO
      IOFF=IOFF+LF
      IW(IOFF+1)=NCOLS
      NCOLS2=NCOLS
      INDYY=IOFF+2
 
*     For "normal" formats
*     translate internal format definition in IFMT into
*     set of parameters stored in workbank.
 
      IF(LFBIT.EQ.2)THEN
*       mark B08/16/32 banks via NCOLS2=-NCOLS
        IW(IOFF+2)=-NCOLS
      ELSE
        CALL NXTFMT(IFMT(2),0,N,K)
* count number of columns defined
        NCIN=0
70      CALL NXTFMT(IFMT(2),NFMT-1,NWORD,KF)
*       check if all columns defined; allow for trailing column subtraction
*       information to be appended
        IF(NCIN.GE.NCOLS.AND.KF.NE.6)GOTO 80
        IF(KF.EQ.4)THEN
* 'Gn.m' format
* potentially still to be done: setting N=999999999 destroys information here
* change NXTFMT for this ?  NREP=MIN(NWORD,NCOLS-NCIN)
          IF(NWORD.EQ.999999999)THEN
            WRITE(6,*)' NXTFMT still to be modified for this format'
            stop
          ENDIF
          NNN=NWORD/1000
          NREP=NWORD-NNN*1000
*         one additonal column for each G column
          NCOLS2=NCOLS2+NREP
          MMM=NNN/100
          NNN=NNN-100*MMM
          IF(NNN.LT.1.OR.NNN.GT.6)THEN
            WRITE(6,*)' 1<=N<=6 needed in Format Gn.m :',FMT, NABANK
            STOP
          ENDIF
          IF(MMM.LT.1.OR.MMM.GT.20)THEN
            WRITE(6,*)' 1<=M<=20 needed in Format Gn.m :',FMT, NABANK
            STOP
          ENDIF
* chosse MMM, such that 2**-MMM < 10**-m
          MMM=IFIX(MMM/LOG10(2.))+1
* the number of significant bits for mantisse
          NF=IFIX(NNN/LOG10(2.))+1
          DO II=1,NREP
            IW(INDYY+1)=23-NF
            IW(INDYY+2)=127-MMM+NF-1
            IW(INDYY+3)=0
            INDYY=INDYY+3
          ENDDO
          NCIN=NCIN+NREP
        ELSE IF(KF.EQ.5)THEN
*  'Y' format
*         two additional columns for each Y column
          NREP=MIN(NWORD,NCOLS-NCIN)
          NCOLS2=NCOLS2+2*NREP
          DO II=1,NREP
            IW(INDYY+1)=-1
            IW(INDYY+2)=0
            IW(INDYY+3)=0
            INDYY=INDYY+3
          ENDDO
          NCIN=NCIN+NREP
        ELSE IF(KF.EQ.6)THEN
*   define column to be subtracted before compression
*   warning: this works currently only, if the subtracted column
*   has a LOWER column number than the current column.
*   also currently only subtraction ('-' sign) allowed, i.e
*   N has to be even.
          IF(MOD(NWORD,2).EQ.1)THEN
            WRITE(6,*)' Adding of columns not yet supported:',NABANK
            STOP
          ELSE
            NCSUB=NWORD/2
            IF(NCSUB.GE.NCIN-NREP+1)THEN
              WRITE(6,*)'Subtracted column must be on left side:',NABANK
              STOP
            ELSE
              INDYY=INDYY-NPAR*NREP
              DO II=1,NREP
                INDYY=INDYY+NPAR
                IW(INDYY)=NCSUB
              ENDDO
            ENDIF
          ENDIF
        ELSE IF(KF.EQ.1)THEN
*  'F' format: keep full floating point precision (all F transformed to IEEE)
          NREP=MIN(NWORD,NCOLS-NCIN)
          DO II=1,NREP
            IW(INDYY+1)=-2
            IW(INDYY+2)=0
            IW(INDYY+3)=0
            INDYY=INDYY+3
          ENDDO
          NCIN=NCIN+NREP
        ELSE
*  everything else assumed to be I in the moment....
          NREP=MIN(NWORD,NCOLS-NCIN)
          DO II=1,NREP
            IW(INDYY+1)=0
            IW(INDYY+2)=0
            IW(INDYY+3)=0
            INDYY=INDYY+3
          ENDDO
          NCIN=NCIN+NREP
        ENDIF
*       all columns defined  now  ?
        GOTO 70
 
*   update NCOLS2
80      IW(IOFF+2)=NCOLS2
 
*     end of "normal format"
      ENDIF
 
*     okay, job is finished for FMTDEF
      GOTO 1000
 
 
      ENTRY FMTCUR(NABA,LCOLS,LCOLS2)
************************************************************************
* Define "current" bank; check if compression defined                  *
* Input: NABA: bank name as integer                                    *
* Output: LCOLS: number of columns in uncompressed bank                *
*                -1 means that no compression format is defined        *
*         LCOLS2: number of columns in "compressed" bank               *
*                 if LCOLS2=-LCOLS the this is a B08/16/32 bank        *
* This call initializes the access to the column wise information      *
* via FMTNXT.                                                          *
************************************************************************
* potentially still to be done: use TLU here
 
      DO LBANK=1,NBANK
        IF(NABA.EQ.NAMBNK(LBANK))GOTO 120
      ENDDO
      LCOLS=-1
      GOTO 1000
 
120   INXX=IND1(LBANK)
      LF=(IW(INWRK+INXX)-1)/32+1
      INXX=INXX+LF+2
      LCOLS=IW(INWRK+INXX-1)
      LCOLS2=IW(INWRK+INXX)
      GOTO 1000
 
      ENTRY FMTSTA(NDATO,NDAT)
************************************************************************
*  Collect statisctics for current bank:
*  Input: NDATO: the number of 32 bit words before compression         *
*         NDAT : dito after compression
************************************************************************
 
      IF(LBANK.EQ.NBANK+1)THEN
        NTOTO(0)=NTOTO(0)+NDATO
        NTOT (0)=NTOT (0)+NDAT
        NCOUNT(0)=NCOUNT(0)+1
      ELSE
        NTOTO(LBANK)=NTOTO(LBANK)+NDATO
        NTOT (LBANK)=NTOT (LBANK)+NDAT
        NCOUNT(LBANK)=NCOUNT(LBANK)+1
      ENDIF
 
      GOTO 1000
 
      ENTRY FMTPRT
************************************************************************
* Print compression statistics.                                        *
************************************************************************
 
      WRITE(6,'(A//)')' Statistics of bank compression:'
      WRITE(6,*)' Bank  number    size      size    ratio      %'
      WRITE(6,*)'               compressed  normal        of datavolume'
      WRITE(6,*)' -----------------------------------------------------'
      NTOTSU=0
      NTOTSO=0
      NCOUSU=0
      DO I=0,NBANK
        NTOTSU=NTOTSU+NTOT(I)
        NTOTSO=NTOTSO+NTOTO(I)
        NCOUSU=NCOUSU+NCOUNT(I)
      ENDDO
      DO I=1,NBANK
        IF(NCOUNT(I).GT.0.AND.NTOT(I).GT.0)THEN
        WRITE(6,980)
     +       CHAINT(NAMBNK(I)),NCOUNT(I),
     +       FLOAT(NTOT(I))/NCOUNT(I),FLOAT(NTOTO(I))/NCOUNT(I),
     +       FLOAT(NTOTO(I))/NTOT(I),FLOAT(NTOT(I))*100./NTOTSU
        ENDIF
      ENDDO
      IF(NCOUNT(0).GT.0.AND.NTOT(0).GT.0)THEN
        WRITE(6,980)
     +       'REST',NCOUNT(0),
     +       FLOAT(NTOT(0))/NCOUNT(0),FLOAT(NTOTO(0))/NCOUNT(0),
     +       FLOAT(NTOTO(0))/NTOT(0),FLOAT(NTOT(0))*100./NTOTSU
      ENDIF
      IF(NCOUSU.GT.0.AND.NTOTSU.GT.0)THEN
        WRITE(6,980)
     +       'Sum:',NCOUSU,
     +       FLOAT(NTOTSU)/NCOUSU,FLOAT(NTOTSO)/NCOUSU,
     +       FLOAT(NTOTSO)/NTOTSU,FLOAT(NTOTSU)*100./NTOTSU
      ENDIF
 
980   FORMAT(1X,A4,2X,I7,2F10.1,F6.1,F8.1)
      GOTO 1000
 
      ENTRY FMTDDL
************************************************************************
* Print bank DDLs + compression parameters for all FMTDEF defined banks*
************************************************************************
 
      WRITE(6,'(A//)')' DDL description of banks:'
      WRITE(6,*)
     * ' Bank    column   DDL-FMT Comp.-FMT rel.error abs.error'
      WRITE(6,*)' -----------------------------------------------'
 
      DO I=1,NBANK
        NAM=CHAINT(NAMBNK(I))
        WRITE(6,'(1X,A4)')NAM
        LFBIT  =IW(INWRK+IND1(I))
        LF=(LFBIT-1)/32+1
        NC     =IW(INWRK+IND1(I)+1+LF)
        DO ICOL=1,NC
          RERR=0.
          AERR=0.
          IF(LFBIT.EQ.2)THEN
            FORM=FMTB16(IW(INWRK+IND1(I)+1))
          ELSE
            NHELP=IW(INWRK+IND1(I)+2+LF+(ICOL-1)*NPAR+1)
            MHELP=IW(INWRK+IND1(I)+2+LF+(ICOL-1)*NPAR+2)
            IF(NHELP.EQ.0)THEN
              FORM='I'
            ELSE IF(NHELP.EQ.-1)THEN
              FORM='Y'
            ELSE IF(NHELP.EQ.-2)THEN
              FORM='F'
            ELSE
              NF=23-NHELP
              NNN=NF*LOG10(2.)
              MMM=127+NF-1-MHELP
              MMM=MMM*LOG10(2.)
              RERR=10.**(-NNN)
              AERR=10.**(-MMM)
              WRITE(FORM,'(A1,I1,A1,I1)')'G',NNN,'.',MMM
            ENDIF
          ENDIF
cboy          CALL COLNAM(NAM,ICOL,CONAME,FRMDDL)
cboy          WRITE(6,'(6X,I3,1X,A8,5X,A4,2X,A8,2(1PE10.1))')
cboy     +         ICOL,CONAME,FRMDDL,FORM,RERR,AERR
 
        ENDDO
      ENDDO
 
      GOTO 1000
 
      ENTRY FMTNXT(NOFF,IEMIN,NS)
************************************************************************
* Return compression parameters for next column of current bank.       *
* Output: NOFF: determines the number of bits used for mantissa
*               0: Integer values
*              -1: special Y format for strange correlat. coeff.
*         IEMIN: minimal value for IEEE exponent
*         NS     : column index of column to be compared
************************************************************************
 
      NOFF  =IW(INWRK+INXX+1)
      IEMIN =IW(INWRK+INXX+2)
      NS    =IW(INWRK+INXX+3)
 
      INXX=INXX+NPAR
      GOTO 1000
 
      ENTRY FMTFMT(LFMTS,LFBITS)
************************************************************************
* get compressed format information for current bank
************************************************************************
 
      INX=INWRK+IND1(LBANK)
      LFBITS=IW(INX)
      LF=(LFBITS-1)/32+1
      DO I=1,LF
        LFMTS(I)=IW(INX+I)
      ENDDO
      GOTO 1000
 
1000  RETURN
 
9999  WRITE(6,*)'FMTDEF: problems with workbank'
      STOP
      END
