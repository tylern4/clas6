*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      INTEGER FUNCTION IFPTRB()
*
************************************************************************
*
*     Test on error history
*
*     Return the depth of the error history
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
      IFPTRB = LTRCE
*
      RETURN
      END
