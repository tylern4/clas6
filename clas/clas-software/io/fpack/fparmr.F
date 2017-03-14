*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPARMR(LUN)
*
************************************************************************
*
*     Read statements from the unit LUN and execute them
*
************************************************************************
*
      CHARACTER TEXT*80
*
      CALL FINITL
*
   10 READ(LUN,END=100,FMT='(A)') TEXT
*
      I = INDEX(TEXT,'END')
*
      IF(I.EQ.0) GOTO 40
      IF(I.EQ.1) GOTO 20
      IF(TEXT(1:I-1).NE.' ') GOTO 40
   20 LENTXT=LENB(TEXT)
      IF(I+3.GT.LENTXT) GOTO 100
      IF(TEXT(I+3:LENTXT).EQ.' ') GOTO 100
*
   40 CALL FPARM(TEXT)
      GOTO 10
*
  100 RETURN
      END
