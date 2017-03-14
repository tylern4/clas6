*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPSIDX(IARR,MES)
*
      CHARACTER*(*) MES
      INTEGER IARR(*)
*
      WRITE(*,*) 'FPSIDX: ',MES
      WRITE(*,*) '  RECORDS',IARR(11),IARR(19),IARR(18)
      WRITE(*,*) '  REFER  ',IARR(17)
      WRITE(*,*) '  POS NW ',IARR(15),IARR(22)
      WRITE(*,*) '  STAMP  ',IARR(8) ,IARR(9)
      IF( IARR(22).LT.7 ) THEN
         WRITE(*,*) '    EMPTY'
      ELSE
         JP = 13
         KP = JP + IARR(JP+1)
         WRITE(*,*) '     KP',KP
         DO I=0,IARR(22)-7,7
            CALL FPKIDX(IARR(KP+I+1))
            WRITE(*,*) '         ',IARR(KP+I+5),IARR(KP+I+6),
     +                             IARR(KP+I+7)
         ENDDO
      ENDIF
*
      RETURN
      END
