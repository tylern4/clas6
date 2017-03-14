CDECK  ID>, FRBOS.
      SUBROUTINE FRBOS(JW,LUN,LIST,IER)
c
c_begin_doc
c  RCS ID string
c  $Id: frbos.F,v 1.1.1.1 1997/03/28 20:55:45 marki Exp $
c
c  Purpose:
c  --------
c              read from unit LUN
c        Read one BOS event (=logical record).
c        Compressed banks are expanded to full banks
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "bcscom.inc"
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),LUN,IER
      CHARACTER*(*) LIST
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: frbos.F,v 1.1.1.1 1997/03/28 20:55:45 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FRBOS.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NUM1,NUM2,NDATWD,IRD,ISTART,ICOUNT,NR,NCOL,NROW,NF,ICH
      INTEGER MBANK,MDROP,NREAD,NDIM,NDATA,IKY
      CHARACTER*(*) NAMKEY*8, FORMAT*255, NAMDAT*8
      CHARACTER*4 KEYB, CHA
      DATA KEYB/'+KYE'/
c_end_var
c----6----------------------------------------------------------------72
      IER=0
*     switch to unit
      CALL FRUNIT(LUN)           ! select unit for input by unit number
      CALL WGARB(JW)  !   something wrong here !!!
*
      CALL FRKEY2(NAMKEY,NUM1,NUM2,NDATWD,IER)  ! return next key block
c      WRITE(*,112) NAMKEY,NUM1,NUM2,NDATWD,IER
c  112 FORMAT(' FRKEY ',A8,4I6)
      IF(IER.NE.0) GOTO 50
C define key for list LIST
      CALL FWBOSK(JW,LIST,NAMKEY,NUM1,NUM2,NDATWD)
C drop +REC bank, if present
      IRD=MDROP(JW,'+REC',LUN)
      ISTART=0
      ICOUNT=0
 
C begin reading loop
 
   10 CALL FRHDR(NAMDAT,NR,NCOL,NROW,FORMAT,NF,IER)   ! get data header
c      WRITE(*,111) NAMDAT,NR,NCOL,NROW,IER,NF,FORMAT(1:NF)
c  111 FORMAT(' FRBOS ',A8,5I5,1X,A)
      IF(IER.NE.0) GOTO 20                            ! end-of-file (?)
      CHA =NAMDAT
*  char*4 --> hollerith 4H
      CALL UCTOH(CHA,ICH,4,4)
      IF(NF.GT.0.AND.NCOL.GT.0) THEN
         IF(FORMAT(1:NF).NE.' ') THEN
*           store bank format
            CALL BKFMT(CHA,FORMAT(1:NF))
         END IF
      END IF
compress*     expected number of words
compress      NCOUNT=NCOL*NROW
      IF(ISTART.EQ.0) THEN
         IRD=MBANK(JW,'+REC',LUN,0,1)
         IF(IRD.EQ.0) GOTO 90
         ISTART=JW(IGP)                 ! first index of gap
      END IF
      JW(ISTART+NHW-6)=NCOL
      JW(ISTART+NHW-5)=NROW
      JW(ISTART+NHW-4)=ICH
      JW(ISTART+NHW-3)=NR
      JW(ISTART+NHW-2)=0
 
compress
 
      NDIM=JW(IWK)-ISTART-NHW                   ! what space we have ?
      IF(NDIM.LE.200) GOTO 90                   ! too little space
      CALL FRDAT(NREAD,JW(ISTART+NHW),NDIM)     ! read data
      NDATA=NREAD
      JW(ISTART+NHW-1)=NDATA
* now the bank is read, check if to be decompressed
      IF(NCOL.LT.0) THEN
cboy      print *,'unsqez reached !'
        NCOL=-NCOL
* unpack to intermediate workbank
        CALL UNSQEZ(JW,ISTART+NHW-1,NCOL,NROW,*90)
* check if enough space for full bank
        IF(ISTART+NHW-1+NCOL*NROW.GE.JW(IWK)-200) GOTO 90
* create "original" bank taking into account compression parameters;
* translate compressed format representation to original BOS bank
* format (including e.g. B16 definitions)
*    still to be solved: FITOFT works only on IW array !
        CALL FITOFT(ISTART+NHW-1)
        NDATA=JW(ISTART+NHW-1)
cboy        write(6,*) ' gap:',ISTART+NHW-1+NDATA,JW(IWK)
      ENDIF
* prepare storage of next bank
      ISTART=ISTART+NDATA+NHW                   ! move free space index
      ICOUNT=ICOUNT+NDATA+NHW
compress      JW(ISTART+(NHW-1))=NDATA
      IF(NREAD.LT.NDIM) GOTO 10
 
C end reading loop
 
*     insufficient space to store full record
      IRD=MDROP(JW,'+REC',LUN)
      GOTO 90
*     check internal structure and determine name indices
   20 JW(1)=1
      IRD=MBANK(JW,'+REC',LUN,ICOUNT,1)
      IF(IRD.EQ.0) THEN
         IRD=MDROP(JW,'+REC',LUN)
         IKY=MDROP(JW,KEYB,0)
         GOTO 90
      END IF
      CALL BBNIN(JW,LUN)
      IF(JW(2).NE.0) GOTO 90
*     define list and insert pointers
      CALL BBINS(JW,LUN,LIST)
      IF(IER.LT.0) IER=0
      IF(IER.GT.0) THEN
*       drop banks from E and R list and reset lists
        CALL BDROP(JW,'E')
        CALL BDROP(JW,'R')
*       drop HEAD bank always (drop only HEAD bank from S list)
        CALL BDROP(JW,'HEAD')
        CALL BLIST(JW,'E=','0')
        CALL BLIST(JW,'S=','0')
        CALL BLIST(JW,'R=','0')
*       drop key banks of 'E' and 'S' lists
        IKY=MDROP(JW,'+KYE',0)
        IKY=MDROP(JW,'+KYS',0)
      ENDIF
      GOTO 100
*     end of file
   50 JW(2)=0
      IRD=MDROP(JW,'+REC',LUN)
      GOTO 100
   90 IER=1
  100 CONTINUE
      RETURN
      END
