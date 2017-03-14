*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRCONV(IREC,IFM,IER)
*
      INTEGER IREC(*)
*
************************************************************************
*
*     Conversion of complete record to format IFM (0, 1, 2, 3, 4 ...)
*     (0 = local format, 1 2 3 4 are IEEE IBM VAX DECS formats)
*     EXCEPT the 10 words of record key
*
*     Convert all headers to local format, otherwise cannot be used !
*
************************************************************************
*
*KEEP,MACHLOC.
#ifndef Linux
*     IEEE format
      PARAMETER (LOCAL=1)
#else /* Linux */
*     DEC format (little-endian IEEE)
      PARAMETER (LOCAL=4)
#endif /* Linux */
*KEEP,FBUFFER.
      PARAMETER (NBUF=100000)
*KEND.
      INTEGER IFMTH(3),IFMTR(3),IFMTS(3),IFMTA(1),IFMT(200)
      CHARACTER*72 ERRSTR
      LOGICAL START
      DATA START/.TRUE./
*
      IF(START) THEN
         START=.FALSE.
         CALL FTCTOI('(I)',     IFMTH,NFMTH)
         CALL FTCTOI('3I,2A,6I',IFMTR,NFMTR)
         CALL FTCTOI('I,2A,6I', IFMTS,NFMTS)
         CALL FTCTOI('A',       IFMTA,NFMTA)
      END IF
*     reset error flag
      IER=0
*     KFM is target word format
      KFM=IFM
      IF(KFM.EQ.0) KFM=LOCAL
      JFM=IREC(4)/16
*     convert 2-WORD record header
      CALL FCNVR(JFM,KFM,IREC(1),2,0,IFMTH,NFMTH)
*     init pointer I to record segment header
      I=2
*     test end-of-record
   10 IF(I.EQ.IREC(2)) GOTO 100
*     pointer to next segment in same record
*     range check, corrupted files might crash the routine otherwise
      if (I+11.gt.NBUF)   GOTO 90
      INEXT=(I+11)+IREC(I+11)
*     formal data tests
      IF(I.GT.IREC(2))    GOTO 90
      IF(I+11.GT.IREC(2)) GOTO 90
*     get word format of record
      JFM=IREC(I+2)/16
*     skip to next segment for diagonal term (no conversion)
      IF(JFM.EQ.KFM) GOTO 30
*     word format conversion of segment header from JFM to KFM
*     first 10 words not converted, only 11th
      CALL FCNVR(JFM,KFM,IREC(I+11),1,0,IFMTH,NFMTH)
*     change internal word format flag
      IREC(I+2)=16*KFM+MOD(IREC(I+2),16)
*     pointer J to first data segment header
      J=I+11
*     next data segment header
   20 IF(J.EQ.INEXT) GOTO 30
*     check data segment pointer for formal errors
      IF(IREC(J+1).LT.10.OR.J+IREC(J+1).GT.IREC(2)) GOTO 90
*     convert data segment header at J
      CALL FCNVR(JFM,KFM,IREC(J+1),9,0,IFMTS,NFMTS)
*     indices for format code
      IHF=J+10
      NHF=IREC(J+1)-9
      IF(JFM.EQ.LOCAL) THEN
*        convert format in local repr. into integers
         CALL FTHTOI(IREC(IHF),NHF,          IFMT,NFMT)
*        convert format code to target repr
         CALL FCNVR(JFM,KFM,IREC(IHF),NHF,0,IFMTA,NFMTA)
      ELSE IF(KFM.EQ.LOCAL) THEN
*        convert format code to target repr
         CALL FCNVR(JFM,KFM,IREC(IHF),NHF,0,IFMTA,NFMTA)
*        convert format in local repr. into integers
         CALL FTHTOI(IREC(IHF),NHF,          IFMT,NFMT)
      ELSE
*        convert format code to local repr
         CALL FCNVR(JFM,LOCAL,IREC(IHF),NHF,0,IFMTA,NFMTA)
*        convert format in local repr. into integers
         CALL FTHTOI(IREC(IHF),NHF,          IFMT,NFMT)
*        convert format code to target repr
         CALL FCNVR(LOCAL,KFM,IREC(IHF),NHF,0,IFMTA,NFMTA)
      END IF
*
*     number of words in previous/current subsegments
      NSP=IREC(J+8)
      NSC=IREC(J+9)
*     pointer K to data
      K=J+IREC(J+1)
      IF(K.EQ.INEXT) GOTO 30
*     check data pointer
      CALL FCNVR(JFM,KFM,IREC(K+1),NSC,NSP,IFMT,NFMT)
      J=J+IREC(J+1)+IREC(J+9)
      GOTO 20
*     next record segment
   30 I=INEXT
      GOTO 10
   90 WRITE(ERRSTR,101) I,J,K
      IER=1
      CALL FPWMSG(IER,'FRCONV',ERRSTR)
*
  100 RETURN
  101 FORMAT('Bad FPACK-record: wrong internal pointers I J K =',3I6)
      END
