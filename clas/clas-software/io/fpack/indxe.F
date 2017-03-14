*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION INDXE(IAR,IDENT,ND)
*
      INTEGER IAR(*)
*
************************************************************************
*
*     Expansion of bank IDENT, keeping the contents.
*
************************************************************************
*
      PARAMETER(INDCPW=999999999)
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
*
      IF(IAR(2).EQ.0.OR.ND.LE.0) GOTO 90
*
*     try find index IND of bank with IDENT by loop through array
*
      IND=INDXB(IAR,IDENT)
*
*     bank to be created ...  does not exist
      IF(IND.EQ.0) GOTO 90
      IF(ND.LE.IAR(IND-3)) THEN
*        .. is existing with sufficient space
         IAR(IND)=ND
         GOTO 100
      ELSE IF(IAR(2).EQ.IND+IAR(IND-3)) THEN
*        last bank, try to increase
         IF(IAR(2)+ND-IAR(IND-3).LE.IAR(1)) THEN
*           change length of last bank directly
            IAR(2)=IAR(2)+ND-IAR(IND-3)
            IAR(IND-3)=ND
            IAR(IND)=ND
            GOTO 100
         END IF
      END IF
*
      N = IAR(IND)
      IND9 = INDXA(IAR,INDCPW,N)
      IF(IND9.LE.0) CALL LOGERROR('NO SPACE FOR BANK EXPANSION')
      IND  = INDXB(IAR,IDENT)
*
      DO 1 I=1,N
         IAR(IND9+I) = IAR(IND+I)
 1    CONTINUE
*
      IND = INDXA(IAR,IDENT,ND)
      IF(IND.LE.0) CALL LOGERROR('NO SPACE FOR BANK EXPANSION')
      IND9 = INDXB(IAR,INDCPW)
*
      DO 2 I=1,N
         IAR(IND+I) = IAR(IND9+I)
 2    CONTINUE
*
      IND9 = INDXA(IAR,INDCPW,-1)
*
  100 INDXE=IND
      RETURN
   90 INDXE = INDXA(IAR,IDENT,ND)
      RETURN
      END
