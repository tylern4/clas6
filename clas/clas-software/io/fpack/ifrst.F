*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION IFRST(ITEM,IAD)
*
      CHARACTER*(*) ITEM
*
************************************************************************
*
*     Integer FRom STring ITEM for IAD >= 0
*        if ITEM is in list of strings, the correponding integer is ret.
*        if ITEM is not in list
*           and IAD=0, then zero is returned
*           and not IAD=0, then ITEM,IAD is stored and IAD is returned
*
*     String from integer for IAD < 0
*        if -IAD is in list, ITEM is return via argument
*           and the value -IAD is returned
*        if -IAD is not in list, 0 is returned
*
************************************************************************
*
      PARAMETER (NDIM=20)
      CHARACTER*16 STR(NDIM), JTEM
      INTEGER      IVL(NDIM)
      DATA N/0/
*
      IF(IAD.LT.0) GOTO 30
      IENTRY=1
*
   01 JTEM=ITEM
*     binary search
      IF(N.EQ.0) THEN
         I=0
      ELSE IF(LLT(JTEM,STR(1))) THEN
         I=0
      ELSE IF(LGT(JTEM,STR(N))) THEN
         I=N
      ELSE
         IB=N+N
         I =1
   10    IF(JTEM.EQ.STR(I)) THEN
            IFRST=IVL(I)
*           found
            IF(IENTRY.EQ.1) GOTO 100
*           remove the entry
            DO 15 J=I+1,N
            STR(J-1)=STR(J)
            IVL(J-1)=IVL(J)
   15       CONTINUE
            N=N-1
            GOTO 100
         END IF
         IF(LGT(JTEM,STR(I))) THEN
            IA=I
         ELSE
            IB=I
         END IF
         I=(IA+IB)/2
         IF(I.NE.IA) GOTO 10
      END IF
*     not found ...
      IF(IAD.EQ.0) THEN
*        ... return zero
         IFRST=0
      ELSE
*        ... add to (sorted) list
         IF(N.EQ.NDIM) THEN
            IFRST=0
         ELSE
*           shift high part of list
            DO 20 J=N,I+1,-1
            STR(J+1)=STR(J)
   20       IVL(J+1)=IVL(J)
            N=N+1
*           add new entry in sorted order
            STR(I+1)=JTEM
            IVL(I+1)=IAD
            IFRST=IAD
         END IF
      END IF
      GOTO 100
*
*     string from integer (for negative argument)
   30 DO 40 J=1,N
      IF(IAD+IVL(J).EQ.0) THEN
         ITEM=STR(J)
         IFRST=-IAD
         GOTO 100
      END IF
   40 CONTINUE
      IFRST=0
      GOTO 100
*
      ENTRY IFRSTD(ITEM,IAD)
*
*     delete the pair ITEM = corresp. integer
*
      IENTRY=2
      GOTO 01
*
  100 RETURN
      END
