*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE PNVI(NUM,STR,JS)
*
      CHARACTER*(*) STR
*
************************************************************************
*
*     Convert integer NUM into character string STR with JS non-blank
*     characters. JS = 1 ... 12
*     Examples:  CALL PNVI( -1,STR,JS)   -> STR='-1' ; JS = 2
*                CALL PNVI(317,STR,JS)   -> STR='317' ; JS = 3
*
************************************************************************
*
#if defined(IRIX)
      CHARACTER*20 TEMP
#endif
      CHARACTER*12 DIG(0:9)*1
      INTEGER ND(12)
      DATA DIG/'0','1','2','3','4','5','6','7','8','9'/
      JS=0
*
#if defined(IRIX)
*
*     check for NaN
      WRITE(TEMP,'(I20)') NUM
      IF(INDEX(TEMP,'NaN').NE.0.OR.INDEX(TEMP,'nan').NE.0) THEN
         STR = 'NaN'
         JS  = 3
         RETURN
      END IF
*     check for Infinity
      IF(INDEX(TEMP,'Infinity').NE.0) THEN
         STR = 'Inf'
         JS  = 3
         RETURN
      END IF
#endif
      K=IABS(NUM)
      DO 10 I=1,12
      ND(I)=MOD(K,10)
      K    =K/10
      IF(K.EQ.0) GOTO 12
   10 CONTINUE
      I=12
   12 STR=' '
      IF(NUM.LT.0) THEN
         JS=JS+1
         STR(JS:JS)='-'
      END IF
      DO 14 J=I,1,-1
      JS=JS+1
   14 STR(JS:JS)=DIG(ND(J))
*
      RETURN
      END
