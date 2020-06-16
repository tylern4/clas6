*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCNVR(JFM,KFM,ARRAY,N,NSKIP,IFMT,NFMT)
*
      INTEGER ARRAY(*),IFMT(*)
*
************************************************************************
*
*     General conversion from word format JFM to KFM
*
************************************************************************
*
*     skip diagonal terms
      IF(JFM.EQ.KFM) GOTO 100
*
*     non-diagonal terms
      IF(JFM.EQ.1) THEN
         IF(KFM.EQ.2) THEN
            CALL CNVR12(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.3) THEN
            CALL CNVR13(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.4) THEN
            CALL CNVR14(ARRAY,N,NSKIP,IFMT,NFMT)
         END IF
      ELSE IF(JFM.EQ.2) THEN
         IF(KFM.EQ.1) THEN
            CALL CNVR21(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.3) THEN
            CALL CNVR23(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.4) THEN
            CALL CNVR24(ARRAY,N,NSKIP,IFMT,NFMT)
         END IF
      ELSE IF(JFM.EQ.3) THEN
         IF(KFM.EQ.1) THEN
            CALL CNVR31(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.2) THEN
            CALL CNVR32(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.4) THEN
            CALL CNVR34(ARRAY,N,NSKIP,IFMT,NFMT)
         END IF
      ELSE IF(JFM.EQ.4) THEN
         IF(KFM.EQ.1) THEN
            CALL CNVR41(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.2) THEN
            CALL CNVR42(ARRAY,N,NSKIP,IFMT,NFMT)
         ELSE IF(KFM.EQ.3) THEN
            CALL CNVR43(ARRAY,N,NSKIP,IFMT,NFMT)
         END IF
      END IF
*
  100 RETURN
      END
