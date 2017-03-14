*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FQXNAM(DANAME,RECNAM,XRECNM)
*
      CHARACTER*(*) DANAME, RECNAM, XRECNM
*
************************************************************************
*
*     Get next name in alphabetical order after from FB file(s)
*                       ______ ______
*           CALL FQXNAM(DANAME,RECNAM,XRECNM)
*                                   ------
*     where  DANAME = symbolic file name of keyed access file
*            RECNAM = name of a record (= '   ' for first name)
*     returned:
*            XRECNM = name of records, in alphabetical order directly
*                     behind RECNAM
*                   = '    ' after last name
*
************************************************************************
*
      CHARACTER*8   NAMSC, NAMRC
*
      XRECNM=' '
*
*     connect file
      LUN     =IFRST(DANAME,0)
      IF(LUN.EQ.0) GOTO 100
      CALL FRUNIT(LUN)
*
*     define record header searching for
      NAMSC=RECNAM
      NUMSA=-1
      NUMSB=-1
*
*     forward search for next record
      CALL FRPOSF(NAMSC,NUMSA,NUMSB,IER)
      IF(RECNAM.EQ.' ') THEN
         CALL FURWND(LUNR)
      END IF
      IF(IER.NE.0) GOTO 100
      IF(RECNAM.EQ.' ') THEN
         CALL FURWND(LUNR)
      END IF
   10 CALL  FRKEY(NAMRC,NUMRA,NUMRB,ICL,IER)
      IF(IER.NE.0) GOTO 100
*
*     not found if different name or run type
      IF(NAMRC.EQ.NAMSC) GOTO 10
*
      XRECNM=NAMRC
*
  100 RETURN
      END
