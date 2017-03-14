*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWNAME(DANAME)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Select unit for output (by data_name)
*
************************************************************************
*
      LUN=IFRST(DANAME,0)
      CALL FWUNIT(LUN)
*
      RETURN
      END
