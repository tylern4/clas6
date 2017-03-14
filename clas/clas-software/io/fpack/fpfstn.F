#ifndef Linux
*CMZU:  1.00/13 30/08/95  17.01.14  by  Unknown
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPFSTN(TO,FROM,IPOS)
*
      CHARACTER*(*) TO, FROM
*
************************************************************************
*
*     Get first name from the list of names, starting at IPOS
*
************************************************************************
*
      CHARACTER  STREXT*12
      CHARACTER  ALPHA*26, DIGITS*10
      DATA ALPHA, DIGITS / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', '0123456789' /
      SAVE ALPHA, DIGITS
*
      L = LEN(FROM)
*
*     skip leading blanks
   10 IF(IPOS.LE.L .AND.
     +   (FROM(IPOS:IPOS).EQ.' ' .OR. FROM(IPOS:IPOS).EQ.',')) THEN
         IPOS = IPOS+1
         GOTO 10
      ENDIF
*
      IF(IPOS.GT.L) THEN
         TO = ' '
         RETURN
      ENDIF
*
      ISTA=IPOS
*
*     find name separator
   20 IF(IPOS.LE.L .AND. FROM(IPOS:IPOS).NE.',') THEN
         IPOS = IPOS+1
         GOTO 20
      ENDIF
*
*     remove trailing blanks in a name
   30 IF(FROM(IPOS-1:IPOS-1).EQ.' ') THEN
         IPOS = IPOS-1
         GOTO 30
      ENDIF
*
*     test whether it is an interval
      IF(IPOS-18.LE.ISTA) THEN
         TO = FROM(ISTA:IPOS-1)
      ELSE IF(FROM(IPOS-18:IPOS-18).EQ.'.' .AND.
     +        IFPWNM(FROM(IPOS-17:IPOS-10)).GE.0 .AND.
     +        FROM(IPOS-9:IPOS-9).EQ.'-' .AND.
     +        IFPWNM(FROM(IPOS-8:IPOS-1)).GE.0) THEN
         N1 = IFPWNM(FROM(IPOS-17:IPOS-10))
         N2 = IFPWNM(FROM(IPOS-8:IPOS-1))
         IF(N1.GT.N2) THEN
            WRITE(*,*) ' ***!!!!*** Bad interval: ',FROM(ISTA:IPOS-1)
            TO = ' '
         ELSE IF(N1.EQ.N2) THEN
            TO = FROM(ISTA:IPOS-10)
         ELSE
            TO = FROM(ISTA:IPOS-10)
            N1 = N1+1
            CALL PNVI(N1,STREXT,LENEXT)
            FROM(IPOS-16:IPOS-10) = STREXT(2:LENEXT)
            IPOS = ISTA
         END IF
      ELSE IF(IPOS-8.LE.ISTA) THEN
         TO = FROM(ISTA:IPOS-1)
      ELSE IF(FROM(IPOS-8:IPOS-8).EQ.'.' .AND.
     +        IFPWNM(FROM(IPOS-7:IPOS-5)).GE.0 .AND.
     +        FROM(IPOS-4:IPOS-4).EQ.'-' .AND.
     +        IFPWNM(FROM(IPOS-3:IPOS-1)).GE.0) THEN
         N1 = IFPWNM(FROM(IPOS-7:IPOS-5))
         N2 = IFPWNM(FROM(IPOS-3:IPOS-1))
         IF(N1.GT.N2) THEN
            WRITE(*,*) ' ***!!!!*** Bad interval: ',FROM(ISTA:IPOS-1)
            TO = ' '
         ELSE IF(N1.EQ.N2) THEN
            TO = FROM(ISTA:IPOS-5)
         ELSE
            TO = FROM(ISTA:IPOS-5)
            N1 = N1+1
            K = N1/100 + 1
            FROM(IPOS-7:IPOS-7) = ALPHA(K:K)
            N1 = MOD(N1,100)
            K = N1/10 + 1
            FROM(IPOS-6:IPOS-6) = DIGITS(K:K)
            K = MOD(N1,10)+1
            FROM(IPOS-5:IPOS-5) = DIGITS(K:K)
            IPOS = ISTA
         END IF
      ELSE
         TO = FROM(ISTA:IPOS-1)
      ENDIF
*
*     skip blanks, so if there are no more names, then
*     IPOS will be out of line.
   40 IF(IPOS.LE.L .AND.
     +   (FROM(IPOS:IPOS).EQ.' '.OR.FROM(IPOS:IPOS).EQ.',')) THEN
         IPOS = IPOS+1
         GOTO 40
      ENDIF
*
      RETURN
      END
