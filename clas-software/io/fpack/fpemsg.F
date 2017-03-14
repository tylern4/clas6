*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      SUBROUTINE FPEMSG(IDEPTH)
*
************************************************************************
*
*     Print error messages report
*
*     IDEPTH = depth of the error history
*     If IDEPTH = 0 : no print & clear the error history
*     If IDEPTH =-1 : print the full trace
*     Else print the trace of depth equal the minimum of {LTRCE,IDEPTH}
*
************************************************************************
*
*KEEP,FPDEBUG.
*     Error messages report
*
*     MXTRCE - parameter defining the max. length of error trace
      PARAMETER (MXTRCE=5)
*
*     LTRCE  - current depth of trace
*     ERRCOD - integer array of error numbers
      INTEGER    ERRCOD
      COMMON /FPADBG/ LTRCE,ERRCOD(MXTRCE)
*
*     ERRPRG - array of program names in which the errors
*              have been detected
*     ERRMSG - character array of corresponding error messages
      CHARACTER  ERRPRG*8, ERRMSG*72
      COMMON /FPAMSG/ ERRPRG(MXTRCE),ERRMSG(MXTRCE)
*
*KEND.
*
      IF(IDEPTH.EQ.0) GOTO 100
      IF(IDEPTH.GT.0) LTRCE = MIN(LTRCE,IDEPTH)
      IF(LTRCE.LE.0) THEN
*        WRITE(6,*) 'We have no error messages for You.'
         GOTO 100
      ENDIF
      WRITE(6,*) '================= ERROR HISTORY  (Depth=',LTRCE,') ',
     +           '================='
      DO 10 I=1,LTRCE
         L = LENB(ERRPRG(I))
         IF(L.GT.0 .AND. ERRCOD(I).NE.0) THEN
            WRITE(6,1000) ERRCOD(I),ERRPRG(I)(1:L)
         ENDIF
         L = LENB(ERRMSG(I))
         IF(L.GT.0) WRITE(6,*) ERRMSG(I)(1:L)
   10 CONTINUE
      WRITE(6,*) '=================================================',
     +           '======================'
  100 LTRCE = 0
      RETURN
 1000 FORMAT(' ***ERROR Code ',I5,3X,'in subroutine "',A,'":')
      END
