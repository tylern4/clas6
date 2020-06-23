*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE TINDEX(IAR)
*
      INTEGER IAR(*)
*
      IND=INDXC(IAR,0)
      IF(IND.EQ.0) GOTO 100
C     WRITE(*,*) ' Overview over all banks'
C     WRITE(6,101) IAR(1),IAR(2)
C  10 WRITE(6,102) IAR(IND-2), IAR(IND),IAR(IND-3),IAR(IND-1)
   10 CONTINUE
      IND=INDXC(IAR,IND)
      IF(IND.NE.0) GOTO 10
*
*     next loop
C     WRITE(*,*) ' Content of all banks'
      IND=INDXC(IAR,0)
      IF(IND.EQ.0) GOTO 100
C  20 WRITE(6,102) IAR(IND-2), IAR(IND),IAR(IND-3),IAR(IND-1)
   20 CONTINUE
C     CALL DUMPAR(IAR(IND+1),1,IAR(IND))
      IND=INDXC(IAR,IND)
      IF(IND.NE.0) GOTO 20
*     print whole array
C     CALL DUMPAR(IAR,1,IAR(1))
*
  100 RETURN
C 101 FORMAT(' The small INDXABC: array has length',I6, ' words, used ar
C    +e',I6,' words.'/'   Bank IDENT   number of words             '/
C    +                 '   ----------     used    total    user flag')
C 102 FORMAT(1X,'IDENT=',I12,
C    +   '   Words used',I9,' total',I9,' Flag=',I12)
C 103 FORMAT(1X,2I5,3X,10I10)
      END
