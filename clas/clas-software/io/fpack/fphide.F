*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPHIDE(TEXT,LB)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     Hide the userid and password (in the job output)
*
************************************************************************
*
      ITEXT=0
      IF(LB.LE.0) GOTO 100
*
*     the first '@'
   10 IAT1 = INDEX(TEXT(ITEXT+1:LB),'@')
      IF(IAT1.NE.0) THEN
         IAT1 = IAT1 + ITEXT
         IF(IAT1.GE.LB) GOTO 100
*
*     the second '@'
         IAT2 = INDEX(TEXT(IAT1+1:LB),'@')
         IF(IAT2.NE.0) THEN
            IAT2 = IAT2 + IAT1
            IF(IAT2.GE.LB) GOTO 100
*
*     the separator ' ' (space)
            ISPAC = INDEX(TEXT(IAT2+1:LB),' ')
            IF(ISPAC.EQ.0) ISPAC = 999999
*
*     the separator '"' (double quotes)
            IDBLQ = INDEX(TEXT(IAT2+1:LB),'"')
            IF(IDBLQ.EQ.0) IDBLQ = 999999
*
*     the separator ',' (comma)
            ICOMM = INDEX(TEXT(IAT2+1:LB),',')
            IF(ICOMM.EQ.0) ICOMM = 999999
*
*     the first separator
            ISEP = MIN0(ISPAC,IDBLQ,ICOMM)
*
            IF(ISEP.EQ.999999) THEN
               DO 20 I=IAT1+1,IAT2-1
   20          TEXT(I:I) = '.'
               DO 30 I=IAT2+1,LB
   30          TEXT(I:I) = '.'
            ELSE
               DO 40 I=IAT1+1,IAT2-1
   40          TEXT(I:I) = '.'
               DO 50 I=IAT2+1,IAT2+ISEP-1
   50          TEXT(I:I) = '.'
               ITEXT = IAT2 + ISEP
               IF(ITEXT.LT.LB) GOTO 10
            END IF
*
         END IF
      END IF
*
  100 RETURN
      END
