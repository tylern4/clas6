#ifdef HPUX
*CMZU:  1.00/16 28/02/96  17.13.00  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      SUBROUTINE FPWMSG(IER,SUBR,MESSGE)
*
      CHARACTER*(*) SUBR,MESSGE
*
************************************************************************
*
*     Write error message into error history
*
*     MESSGE = error message
*     SUBR = name of the subroutine, where errors have occure
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
      MESS00 = MESSGE
      IF(LTRCE.LT.0 .OR. LTRCE.GE.MXTRCE) LTRCE=0
      LTRCE=LTRCE+1
      ERRCOD(LTRCE)=IER
      ERRPRG(LTRCE)=SUBR
      L  = 70
      L1 = LEN(ERRMSG(LTRCE))
      L  = MIN(L,L1)
      IF(L.EQ.0) THEN
         ERRMSG(LTRCE)= ' '
      ELSE
         ERRMSG(LTRCE)=MESSGE(1:L)
      ENDIF
      RETURN
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      SUBROUTINE FPWMSG(IER,SUBR,MESSGE)
*
      CHARACTER*(*) SUBR,MESSGE
*
************************************************************************
*
*     Write error message into error history
*
*     MESSGE = error message
*     SUBR = name of the subroutine, where errors have occure
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
      IF(LTRCE.LT.0 .OR. LTRCE.GE.MXTRCE) LTRCE=0
      LTRCE=LTRCE+1
      ERRCOD(LTRCE)=IER
      ERRPRG(LTRCE)=SUBR
      L  = LENB(MESSGE)
*     L1 = LEN(ERRMSG(LTRCE))
*     L  = MIN(L,L1)
      IF(L.EQ.0) THEN
         ERRMSG(LTRCE)= ' '
      ELSE
         ERRMSG(LTRCE)=MESSGE(1:L)
      ENDIF
      RETURN
      END
#elif defined SunOS
*CMZU:  1.00/16 28/02/96  17.13.00  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      SUBROUTINE FPWMSG(IER,SUBR,MESSGE)
*
      CHARACTER*(*) SUBR,MESSGE
*
************************************************************************
*
*     Write error message into error history
*
*     MESSGE = error message
*     SUBR = name of the subroutine, where errors have occure
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
*     d.p. weygand - 06/1998 - add definition of mess00 for SunOS
*
      CHARACTER*132 MESS00
*
      MESS00 = MESSGE
      IF(LTRCE.LT.0 .OR. LTRCE.GE.MXTRCE) LTRCE=0
      LTRCE=LTRCE+1
      ERRCOD(LTRCE)=IER
      ERRPRG(LTRCE)=SUBR
      L  = 70
      L1 = LEN(ERRMSG(LTRCE))
      L  = MIN(L,L1)
      IF(L.EQ.0) THEN
         ERRMSG(LTRCE)= ' '
      ELSE
         ERRMSG(LTRCE)=MESSGE(1:L)
      ENDIF
      RETURN
      END
#else
*CMZU:  1.00/16 28/02/96  17.13.00  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Sergey Esenov   16/12/94
      SUBROUTINE FPWMSG(IER,SUBR,MESSGE)
*
      CHARACTER*(*) SUBR,MESSGE
*
************************************************************************
*
*     Write error message into error history
*
*     MESSGE = error message
*     SUBR = name of the subroutine, where errors have occure
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
*     jam - 04/1996 - add definition of mess00 for aix.
*
      CHARACTER*132 MESS00
*
      MESS00 = MESSGE
      IF(LTRCE.LT.0 .OR. LTRCE.GE.MXTRCE) LTRCE=0
      LTRCE=LTRCE+1
      ERRCOD(LTRCE)=IER
      ERRPRG(LTRCE)=SUBR
      L  = 70
      L1 = LEN(ERRMSG(LTRCE))
      L  = MIN(L,L1)
      IF(L.EQ.0) THEN
         ERRMSG(LTRCE)= ' '
      ELSE
         ERRMSG(LTRCE)=MESSGE(1:L)
      ENDIF
      RETURN
      END
#endif
