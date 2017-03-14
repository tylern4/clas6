*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FNRWND(DANAME)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Rewind (by data_name)
*
************************************************************************
*
      LUN=IFRST(DANAME,0)
      CALL FURWND(LUN)
*
  100 RETURN
      END
