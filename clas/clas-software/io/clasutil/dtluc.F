CDECK  ID>, DTLUC.
*-- Author :    Stephan Egli   09/10/91
      SUBROUTINE DTLUC(NDIM,NEN,TAB,AUX)
*
*     For description see DTLU2
*
*     original Author: V. Blobel Jan 88
*
      CHARACTER*(*)TAB(NDIM)
      INTEGER AUX(2*NDIM)
*     NDIM at least 10,  otherwise stop
      IF(NDIM.LT.10) STOP
*     Clear aux array ...
      DO 10 I=1,2*NDIM
   10 AUX(I)=0
*     ... and define first three words
      AUX(1)=NDIM
      AUX(2)=0
C     AUX(3)=NEN
*     determine prime number for hash function
      NPRIM=NDIM-3
      IF(MOD(NPRIM,2).EQ.0) NPRIM=NPRIM-1
   20 NPRIM=NPRIM-2
      IF(NPRIM.GT.5) THEN
         DO 30 I=3,INT(SQRT(FLOAT(NPRIM))),2
         J=NPRIM/I
         IF(I*J.EQ.NPRIM) GOTO 20
   30    CONTINUE
      END IF
      AUX(2)=NPRIM
*     Loop to insert index structure in AUX for the NEN existing
*     entries in the table
      DO 50 K=1,MIN(NEN,NDIM)
      ILEN=INDEX(TAB(K),' ')-1
      IF(ILEN.EQ.-1)ILEN=LEN(TAB(K))
      IF(ILEN.EQ.1)THEN
        INAME=ICHAR(TAB(K)(1:1))
      ELSE IF (ILEN.EQ.2)THEN
        INAME=ICHAR(TAB(K)(1:1))+ICHAR(TAB(K)(2:2))
      ELSE IF (ILEN.EQ.3)THEN
        INAME=ICHAR(TAB(K)(1:1))+ICHAR(TAB(K)(2:2))+ICHAR(TAB(K)(3:3))
      ELSE
        INAME=ICHAR(TAB(K)(1:1))+ICHAR(TAB(K)(2:2))+ICHAR(TAB(K)(3:3))
     +        +ICHAR(TAB(K)(4:4))
      ENDIF
*     search and add (like function JTLU)
      J=AUX(1)+MOD(INAME,AUX(2))+1
   40 I=J
      J=AUX(I+5)
      IF(J.NE.0) THEN
         IF(TAB(K).NE.TAB(J)) GOTO 40
         AUX(3)=AUX(3)+1
      ELSE
         J=AUX(3)+1
         AUX(3)=J
         AUX(I+5)=J
      END IF
   50 CONTINUE
  100 RETURN
      END
