#ifndef Linux
*CMZU:  1.00/14 22/02/96  15.28.39  by  Evelyne Lebreton
*CMZU:  1.00/13 04/05/95  17.16.15  by  Unknown
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPGTH(HOST,LPH)
*
      CHARACTER*(*) HOST
*
************************************************************************
*                                                                      *
*     Interface to the C routine fpgthc(). Returns current host name   *
*     and its length (negative if error).                              *
*                                                                      *
************************************************************************
*
      INTEGER   fpgthc
      INTEGER*2 STRLEN
      STRLEN=LEN(HOST)
      IRET = fpgthc(HOST,LPH,STRLEN)
*
*     Fill the end of the string HOST with spaces
      HOST(LPH+1:)=' '
*
      RETURN
      END
