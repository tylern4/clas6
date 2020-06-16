CDECK  ID>, FSEQR.
      SUBROUTINE FSEQR(DANAME,IRET)
c
c_begin_doc
c  RCS ID string
c  $Id: fseqr.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $
c
c  Purpose:
c  --------
c     FSEQR for the standard loop record READING like
c     CALL FSEQR(DANAME,IRET)              !! using F-package !!
c     IF(IRET.LT.0) GOTO ... (end-of-data)
c
c     For compatibility with
c     old BOS files also the following call is possible:
c        CALL FSEQP(LIMSEC,LIMREC) with default (2,0)
c
c     assuming: standard array IW/RW in /BCS/
c               list E for input of  RUNEVENT and RUNDATA records
c                      and output of RUNEVENT             records
c                      (banks will     BE DROPPED after writing)
c               list S for output of RUNDATA records
c                      (banks will NOT BE DROPPED after writing)
c               list R to delete banks before new input
c
c     Arguments:
c
c     DANAME = name of data set
c        IRET = returned flag with values ...
c        IRET =  1   record read
c             =  0   end-of-data condition - final loop thru modules
c             = -1   after end-of data condition (skip modules)
c
c     After end-of-data a RESTART of the reading loop is possible,
c     either with another file (DANAME) or with the same file after
c     a rewind (CALL FNRWND(DANAME)).
c
c     Writing:
c     There may be up to four (4) output streams. In order to write
c     the actual banks, the user has to call
c        CALL FSEQW(DANAME)
c     with DANAME = output data name. Writing is delayed until the
c     next call to FSEQR and is done using the list E before the
c     new record is read in.
c
c     User calls to skip a run or to stop the reading loop:
c        CALL FSEQS     to skip the actual run (if some run is defined)
c        CALL FSEQE     to stop the reading loop
c
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
caf#include "boscom.inc"
#include "bcs.inc"
#include "bossys.inc"
#include "bosmdl.inc"
#include "cseqr.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      CHARACTER*(*) DANAME
      INTEGER IRET
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
     1'$Id: fseqr.F,v 1.2 1997/04/12 18:03:26 freyberg Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FSEQR.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER ISPEC,NCR,I,IPFLAG,NOR,IOUTR,IDATE,ITIME,IXTIME,IND,NNNRUN
      INTEGER IERW,J,LASTRU,IKY,ICH,ITFLAG,ISTOP,LUN,IER,NUM1,NUM2
      INTEGER NLINK,MLINK,MDROP,IFRST
*     run numbers and count of events per run
      INTEGER NCRE(2,100)
      CHARACTER*(*) DANAMC*16, CHA*4
      CHARACTER*4 NLIST
      CHARACTER*8 RNAME(10,2)
      INTEGER    NRNAME(10,2),NRN(2),NBITS(30,2)
C ifdef IBMMVS
C      DOUBLE PRECISION CPUA,CPUB
C endif
      LOGICAL START, LENDRU
      DATA START/.TRUE./,LENDRU/.FALSE./,NCRE/200*0/
      DATA ISPEC/0/
c_end_var
c----6----------------------------------------------------------------72
      IF(START) THEN
*        initial entry, reset logical flags ...
        START=.FALSE.
C ifdef IBMMVS
C        CALL CPUTIME(CPUB,IRCODE)
C        NINTR=0
C        SINTR=0.0D0
C        NOUTR=0
C        SOUTR=0.0D0
C        NNNTR=0
C        SNNTR=0.0D0
C        NBBTR=0
C        SBBTR=0.0D0
C endif
        NCR=0
        NRN(1)=0
        NRN(2)=0
        DO 07 I=1,30
          NBITS(I,1)=0
    7   NBITS(I,2)=0
        CALL FSEQST
        CALL FQCOND('PRINT',IPFLAG)
        IF(IPFLAG.NE.0)THEN
          WRITE(*,102)
*          F-PACKAGE logo
          CALL FLOGO
          WRITE(*,105)
          CALL FPRFLS
        ENDIF
*        check open error(s)
        CALL FQCOND('OPENERROR',NOR)
        IF(NOR.NE.0.AND.IPFLAG.NE.0) THEN
          WRITE(*,106)
          GOTO 40
        END IF
      ELSE
        IF(EOD) THEN
*         set return flag for final return
          IRET=-1
*         write banks produced in final call to RUNDATA records
*         (for backward compatibility 'S' AND 'E' lists are written)
          CALL BLIST(IW,'S+','E')
          IF(NLIST(IW,1,'S').NE.' ')THEN
            IOUTR=0
            DO 97 I=1,NLUNW
              IF(LUNT(I).NE.0) THEN
                CALL TIMUYH (1,IDATE,ITIME,IXTIME)
                NAMKEY='RUNDATA'
                IND=NLINK('HEAD',0)
                IF(IND.EQ.0)THEN
                  WRITE(*,112)
                  NNNRUN=0
                ELSE
                  NNNRUN=IW(IND+2)
*                 add HEAD+HEAR bank if not yet done
                  CALL BLIST(IW,'S+','HEADHEAR')
                ENDIF
                CALL FWBOSK(IW,'S',NAMKEY,NNNRUN,IXTIME,0)
                CALL FWBOS(IW,LUNT(I),'S',IERW)
                CALL FWBOS(IW,LUNT(I),'0',IERW)
                IOUTR=IOUTR+1
                IF(IOUTR.EQ.1)THEN
*                 statistics on record names
                  DO 26 J=1,NRN(2)
                    IF(NAMKEY.EQ.RNAME(J,2)) GOTO 27
   26             CONTINUE
                  IF(NRN(2).LT.10) THEN
                    NRN(2)=NRN(2)+1
                    RNAME(NRN(2),2)=NAMKEY
                    NRNAME(NRN(2),2)=0
                    J=NRN(2)
                  ELSE
                    J=10
                    RNAME(J,2)='..(else)'
                    NRNAME(J,2)=0
                  END IF
   27             NRNAME(J,2)=NRNAME(J,2)+1
                ENDIF
              ENDIF
   97       CONTINUE
          ENDIF
*
*   a lot of print out...
*
          CALL FQCOND('PRINT',IPFLAG)
          IF(IPFLAG.NE.0.AND.NRN(1)+NRN(2).NE.0) THEN
            WRITE(*,107)
            DO 86 I=1,MAX0(NRN(1),NRN(2))
              IF(I.GT.NRN(1)) RNAME(I,1)=' '
              IF(I.GT.NRN(1)) NRNAME(I,1)=0
              IF(I.GT.NRN(2)) RNAME(I,2)=' '
              IF(I.GT.NRN(2)) NRNAME(I,2)=0
              WRITE(*,108) RNAME(I,1),NRNAME(I,1),RNAME(I,2),NRNAME(I,
     +        2)
   86       CONTINUE
            WRITE(*,109)
            DO 88 I=1,10
              WRITE(*,110) I,NBITS(I,1),NBITS(I+10,1),NBITS(20+I,1),
     +        I,NBITS(I,2),NBITS(I+10,2),NBITS(20+I,2)
   88       CONTINUE
          END IF
          IF(NCR.NE.0) THEN
*           print run overview
            IF(IPFLAG.GT.0) WRITE(*,103) ((NCRE(I,J),I=1,2),J=1,NCR)
            NCR=0
          END IF
          IF(IPFLAG.GT.0) WRITE(*,104)
C ifdef IBMMVS
C          IF(IPFLAG.GT.0) THEN
C            NINTR=NINTR+1
C            SINTR=SINTR+CPUB-CPUA
C            IF(NINTR.NE.0) WRITE(*,111) 'BOS-Input      ', 0.001*SINTR/
C     +      FLOAT(NINTR)
C            IF(NNNTR.NE.0) WRITE(*,111) 'Module-steering', 0.001*SNNTR/
C     +      FLOAT(NNNTR)
C            IF(NBBTR.NE.0) WRITE(*,111) 'BOS drop etc.  ', 0.001*SBBTR/
C     +      FLOAT(NBBTR)
C            IF(NOUTR.NE.0) WRITE(*,111) 'BOS-Output     ', 0.001*SOUTR/
C     +      FLOAT(NOUTR)
C          END IF
C  111 FORMAT(' Time for ',A15,F8.3,' milliseconds per record')
C endif
          IF(IPFLAG.GT.0) CALL BOSTA
          IF(IPFLAG.GT.0) CALL FPRFLS
          IF(IPFLAG.GT.1) CALL FPSTAT
*
 
cboy*         two calls for module steering
cboy          CALL JOBSUM
cboy          IF(IW(6).NE.0)CALL ERRSUM(IW(6))
 
*         but allow restart (further reading loop)
          START=.TRUE.
*         force reset of initialized variables
          CALL FSEQSF
          GOTO 100
        END IF
        BEGJOB=.FALSE.
*        one call for module book keeping: determine final lenght
*        of MODS/I/O banks
C ifdef IBMMVS
C        CALL CPUTIME(CPUA,IRCODE)
C endif
 
cboy        CALL MODULT
 
C ifdef IBMMVS
C        CALL CPUTIME(CPUB,IRCODE)
C        NNNTR=NNNTR+1
C        SNNTR=SNNTR+CPUB-CPUA
C endif
*       output (if requested)
        IOUTR=0
C ifdef IBMMVS
C        CALL CPUTIME(CPUA,IRCODE)
C endif
 
cboy       IF(LENDRU) THEN
cboy*         bank data from buffer -> S list
cboy          CALL H1MATI(1)
cboy       ELSE
cboy*         run number for calibration
cboy          LASTRU=NCCRUN
cboy          DO 81 J=1,NLUNW
cboy          IF(LUNW(J).GT.0) THEN
cboy*            call H1MATI, if output flag on
cboycboy             CALL H1MATI(0)
cboy             GOTO 82
cboy          END IF
cboy   81     CONTINUE
cboy       END IF
cboy   82  CONTINUE
 
        DO 08 J=1,NLUNW
          IF(LUNW(J).GT.0) THEN
*           remember unit for final writing
            LUNT(J)=LUNW(J)
            IOUTR=IOUTR+1
*           first write banks in 'S' list to RUNDATA record if nonempty
            IF(NLIST(IW,1,'S').NE.' ')THEN
              CALL TIMUYH (1,IDATE,ITIME,IXTIME)
              NAMKEY='RUNDATA'
              IND=NLINK('HEAD',0)
              IF(IND.EQ.0)THEN
                WRITE(*,112)
                NNNRUN=0
              ELSE
                NNNRUN=IW(IND+2)
*               add HEAD+HEAR bank if not yet done
                CALL BLIST(IW,'S+','HEADHEAR')
              ENDIF
              NNNRUN=LASTRU
              CALL FWBOSK(IW,'S',NAMKEY,NNNRUN,IXTIME,0)
              CALL FWBOS(IW,LUNW(J),'S',IERW)
              IF(IOUTR.EQ.1)THEN
*               statistics on record names
                DO 16 I=1,NRN(2)
                  IF(NAMKEY.EQ.RNAME(I,2)) GOTO 17
   16           CONTINUE
                IF(NRN(2).LT.10) THEN
                  NRN(2)=NRN(2)+1
                  RNAME(NRN(2),2)=NAMKEY
                  NRNAME(NRN(2),2)=0
                  I=NRN(2)
                ELSE
                  I=10
                  RNAME(I,2)='..(else)'
                  NRNAME(I,2)=0
                END IF
   17           NRNAME(I,2)=NRNAME(I,2)+1
              ENDIF
            ENDIF
            IF(NLIST(IW,1,'E').NE.' ')THEN
              CALL FWBOS(IW,LUNW(J),'E',IERW)
              IF(IOUTR.EQ.1) THEN
                IKY=MLINK(IW,'+KYE',0)
                IF(IKY.EQ.0) STOP 'FSEQR:Key bank can not be created'
*               get key from bank
                ICH=IW(IKY+1)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
                NAMKEY(1:4)=CHA
                ICH=IW(IKY+2)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
                NAMKEY(5:8)=CHA
*               statistics on record names
                DO 76 I=1,NRN(2)
                  IF(NAMKEY.EQ.RNAME(I,2)) GOTO 77
   76           CONTINUE
                IF(NRN(2).LT.10) THEN
                  NRN(2)=NRN(2)+1
                  RNAME(NRN(2),2)=NAMKEY
                  NRNAME(NRN(2),2)=0
                  I=NRN(2)
                ELSE
                  I=10
                  RNAME(I,2)='..(else)'
                  NRNAME(I,2)=0
                END IF
   77           NRNAME(I,2)=NRNAME(I,2)+1
                DO 78 I=1,30
                  IF(IW(IKY+10+I).NE.0) NBITS(I,2)=NBITS(I,2)+1
   78           CONTINUE
              END IF
            END IF
          END IF
*         reset
          LUNW(J)=-IABS(LUNW(J))
    8   CONTINUE
C ifdef IBMMVS
C        IF(IOUTR.NE.0) THEN
C          CALL CPUTIME(CPUB,IRCODE)
C          NOUTR=NOUTR+IOUTR
C          SOUTR=SOUTR+CPUB-CPUA
C        END IF
C endif
*       drop banks from E and R list and reset lists
C ifdef IBMMVS
C        CALL CPUTIME(CPUA,IRCODE)
C endif
        CALL BDROP(IW,'E')
        CALL BDROP(IW,'R')
*       drop HEAD bank always (drop only HEAD bank from S list)
        CALL BDROP(IW,'HEAD')
        CALL BLIST(IW,'E=','0')
        CALL BLIST(IW,'S=','0')
        CALL BLIST(IW,'R=','0')
*       drop key banks of 'E' and 'S' lists
        IKY=MDROP(IW,'+KYE',0)
        IKY=MDROP(IW,'+KYS',0)
C ifdef IBMMVS
C        CALL CPUTIME(CPUB,IRCODE)
C        NBBTR=NBBTR+1
C        SBBTR=SBBTR+CPUB-CPUA
C endif
*       check user stop condition
        IF(USTOP) THEN
          CALL FQCOND('PRINT',IPFLAG)
          IF(IPFLAG.GT.0) WRITE(*,101) 'user stop requested'
          GOTO 40
        END IF
*       check remaining time
        CALL FQCOND('TIME',ITFLAG)
        IF(ITFLAG.NE.0) THEN
          CALL FQCOND('PRINT',IPFLAG)
          IF(IPFLAG.GT.0) WRITE(*,101) 'time limit reached'
          GOTO 40
        END IF
*       check further STOP conditions
        CALL FQCOND('STOP',ISTOP)
        IF(ISTOP.NE.0) THEN
          CALL FQCOND('PRINT',IPFLAG)
          IF(IPFLAG.GT.0) WRITE(*,101) 'stop condition'
          GOTO 40
        END IF
*       check (old) record limit
        IF(LIMREC.NE.0.AND.NREC.GE.LIMREC) THEN
          IF(IW(6).NE.0) WRITE(IW(6),101) 'record limit reached'
          GOTO 40
        END IF
      END IF
*
*     transition to next record: garbage collection, read ...
*
      IRET=1
 
   10 CONTINUE
C ifdef IBMMVS
C      CALL CPUTIME(CPUA,IRCODE)
C endif
      CALL BGARB(IW)
 
cboy*     check remaining banks in named bank area
cboy      CALL MODULC
 
C ifdef IBMMVS
C      CALL CPUTIME(CPUB,IRCODE)
C      SBBTR=SBBTR+CPUB-CPUA
C endif
*     for blank data name assume bank definition outside
      LUN=-2
      IF(DANAME.EQ.' ') GOTO 33
      DANAMC=DANAME
      CALL UCASE(DANAMC)
*     translate data name to unit number
      LUN=IFRST(DANAMC,0)
      IF(LUN.EQ.0) THEN
*       input unit not existing
        CALL FQCOND('PRINT',IPFLAG)
        IF(IPFLAG.GT.0) WRITE(*,101) 'illegal unit'
        GOTO 40
      END IF
C ifdef IBMMVS
C      CALL CPUTIME(CPUA,IRCODE)
C endif
      CALL FRBOS(IW,LUN,'E',IER)
      IF(IER.LT.0) GOTO 30
C ifdef IBMMVS
C      CALL CPUTIME(CPUB,IRCODE)
C      NINTR=NINTR+1
C      SINTR=SINTR+CPUB-CPUA
C endif
*     try to read again after read error
      IF(IER.GT.0) GOTO 10
   33 NREC=NREC+1
*     reading O.K., reset run and event number
      NRUN=0
      NEVT=0
*     link   key-bank
      IKY=MLINK(IW,'+KYE',0)
      IF(IKY.EQ.0) THEN
        IND=MLINK(IW,'HEAD',0)
        IF(IND.NE.0) THEN
          CALL FWBOSK(IW,'E','RUNEVENT',IW(IND+2),IW(IND+3),0)
        ELSE
          ISPEC=ISPEC+1
          CALL FWBOSK(IW,'E','UNKNOWN',ISPEC,0,0)
        END IF
        IKY=MLINK(IW,'+KYE',0)
        IF(IKY.EQ.0) STOP 'FSEQR: Record key bank can not be created'
      END IF
*     get key from bank
      ICH=IW(IKY+1)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(1:4)=CHA
      ICH=IW(IKY+2)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(5:8)=CHA
      NUM1=IW(IKY+3)
      NUM2=IW(IKY+4)
*     set NRUN and NEVT, REVENT flag
      IF(INDEX(NAMKEY,'EVENT').NE.0) THEN
*        it's and event
        NRUN=NUM1
        NEVT=NUM2
        REVENT=.TRUE.
        OTHDAT=.FALSE.
      ELSE
        REVENT=.FALSE.
        OTHDAT=.TRUE.
        IF(NAMKEY.EQ.'RUNDATA ') THEN
          NRUN=NUM1
          NEVT=NUM2
        ELSE
          NEVT=0
          NRUN=0
        END IF
      END IF
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GE.2)WRITE(*,113)NAMKEY,NRUN,NEVT
*     statistics on record names only if input file exists
      IF(DANAME.EQ.' ')GOTO 69
      DO 66 I=1,NRN(1)
        IF(NAMKEY.EQ.RNAME(I,1)) GOTO 67
   66 CONTINUE
      IF(NRN(1).LT.10) THEN
        NRN(1)=NRN(1)+1
        RNAME(NRN(1),1)=NAMKEY
        NRNAME(NRN(1),1)=0
        I=NRN(1)
      ELSE
        I=10
        RNAME(I,1)='..(else)'
        NRNAME(I,1)=0
      END IF
   67 NRNAME(I,1)=NRNAME(I,1)+1
      DO 68 I=1,30
        IF(IW(IKY+10+I).NE.0) NBITS(I,1)=NBITS(I,1)+1
   68 CONTINUE
   69 CONTINUE
*     run statistic for events
      IF(REVENT) THEN
*       events (run number defined)
        IF(NCR.NE.0.AND.NCRE(1,NCR).EQ.NRUN) THEN
*         increase event counter for same run
          NCRE(2,NCR)=NCRE(2,NCR)+1
        ELSE
          IF(NCR.EQ.100) THEN
*           printout after 100 runs and restart table
            CALL FQCOND('PRINT',IPFLAG)
            IF(IPFLAG.GT.0) WRITE(*,103) NCRE
            NCR=0
          END IF
*         add new run to table
          NCR=NCR+1
          NCRE(1,NCR)=NRUN
          NCRE(2,NCR)=1
        END IF
      END IF
      IF(NRUN.NE.0) THEN
*       flags for run dependent data
        ENDRUN=.FALSE.
        BEGRUN=.FALSE.
        LCCRUN=NCCRUN
        NCCRUN=NRUN
        NEVENT=NEVT
        IF(LCCRUN.EQ.NCCRUN) THEN
          BEGRUN=.FALSE.
          ENDRUN=.FALSE.
        ELSE
          BEGRUN=.TRUE.
          IF(LCCRUN.EQ.0) THEN
            ENDRUN=.FALSE.
          ELSE
            ENDRUN=.TRUE.
          END IF
        END IF
      ELSE
*       no run-dependent record
        ENDRUN=.FALSE.
        BEGRUN=.FALSE.
      END IF
      GOTO 100
*
*     end-of-data
*
   30 CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GT.0) WRITE(*,101) 'EOF on input unit'
*     ------------------------------------------------------------------
*     end condition
*     ------------------------------------------------------------------
   40 IRET=0
      EOD=.TRUE.
*     write output buffers
      DO 45 I=1,NLUNW
        IF(LUNT(I).NE.0) THEN
          CALL FWBOS(IW,LUNT(I),'0',IERW)
        END IF
   45 CONTINUE
*
      LCCRUN=NCCRUN
      IF(LCCRUN.EQ.0) THEN
        ENDRUN=.FALSE.
      ELSE
        ENDRUN=.TRUE.
      END IF
      BEGRUN=.FALSE.
      REVENT=.FALSE.
      OTHDAT=.FALSE.
      ENDJOB=.TRUE.
*     store flag ENDRUN and run number
  100 LENDRU=ENDRUN
      LASTRU=LCCRUN
      CALL FPEMSG(-1)
      RETURN
*     format statements
  101 FORMAT('0BOS--FSEQR-  ',A,I8,A)
  102 FORMAT('0BOS--FSEQR-  START OF FSEQR ========================='/)
  103 FORMAT('0BOS--FSEQR-',5(3X,'RUN-NR...EVTS',2X)/(12X,5(2X,2I7,2X)))
  104 FORMAT('0BOS--FSEQR-  STOP  OF FSEQR +++++++++++++++++++++++++'/)
  105 FORMAT('0BOS--FSEQR-  Comment on record keys used in H1:'/
     +14X,'                             Name....  num1  num2          '/
     +14X,'Records with event data      RUNEVENT  run   event         '/
     +14X,'Records with non-event data  RUNDATA   run   fake  (online)'/
     +14X,'Records with non-event data  RUNDATA   run   Unixtime  (MC)'/
     +14X,'       '/
     +14X,'Check end-of-job output on used record key names'/
     +14X,'       '/
     +14X,'       ')
  106 FORMAT('0BOS--FSEQR-  STOP due to OPEN errors')
  107 FORMAT('0Statistics on record keys and classes'/
     + 11X,'   Input...Count                            Output...Count'/
     + 11X,'----------------                          ----------------')
  108 FORMAT(11X,A8,I8,26X,A8,I8)
  109 FORMAT('                       '/
     +   1X,2('   class     1-10    11-20    21-30       '))
  110 FORMAT(1X,I8,3I9,7X,I8,3I9)
  112 FORMAT(' FSEQR warning:Writing RUNDATA record without HEAD bank'/)
  113 FORMAT(1X,16('='),2X,A8,':  RUN=',I6,'  EVENT=',I10,2X,13('='))
      END
