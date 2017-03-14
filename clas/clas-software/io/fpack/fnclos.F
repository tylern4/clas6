*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FNCLOS(DANAME)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Close (by data_name)
*
************************************************************************
*
      LUN=IFRST(DANAME,0)
      CALL FUCLOS(LUN)
*
  100 RETURN
      END
