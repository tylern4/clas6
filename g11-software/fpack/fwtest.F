*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWTEST(LUN)
*
************************************************************************
*
*     Write one test record
*
************************************************************************
*
      INTEGER IARRAY(100)
      REAL     ARRAY(500)
      DATA     NUM/0/
*
      CALL FWUNIT(LUN)
      NUM=NUM+1
*     definition of key: name nr1 nr2 datawd
      CALL FWKEY('TEXTRECD',1,NUM,0)
*     integer array
      DO 10 I=1,100
   10 IARRAY(I)=I
      CALL FWHDR('INTEGER',0,100,1,'(I)')
      CALL FWDAT(100,IARRAY)
*     floating point array
      DO 20 I=1,500
   20 ARRAY(I)=FLOAT(I)
      CALL FWHDR('FLOATING',0,100,5,'(F,F,2F,F,5F)')
      CALL FWDAT(500,ARRAY)
*
      CALL FWEND(IERR)
      WRITE(6,101) IERR
*
  100 RETURN
  101 FORMAT(' RECORD WRITTEN WITH ERROR CODE ',I4)
      END
