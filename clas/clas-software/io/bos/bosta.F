CDECK  ID>, BOSTA.
      SUBROUTINE BOSTA
c
c_begin_doc
c  RCS ID string
c  $Id: bosta.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $
c
c  Purpose:
c  --------
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
#include "bcs.inc"
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
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
     1'$Id: bosta.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSTA.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MIN,IMAX,I,J,K,MNAM,MGAP,MWRK
c      REAL FDATE
      REAL TLEFTC,SEC
      CHARACTER*4 CHAINT
      LOGICAL START
c+SEQ, DATE.
      DATA START/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
c      FDATE=0.0001*FLOAT(IDATE)
c      IF(START) WRITE(NPR,1006) FDATE
      START=.FALSE.
      CALL TIMEL(TLEFTC)
      SEC=TLEFT-TLEFTC
      MIN=SEC/60.0
      SEC=SEC-FLOAT(MIN)*60.0
      WRITE(NPR,1001) MIN,SEC,TLEFTC
      WRITE(NPR,1002)
      IMAX=1
      DO 10 J=1,NARR
      I=IARR(J)-IARR(1)
      IMAX=MAX0(IMAX,IW(I+LCH))
      MNAM=IW(I+IGP)-IW(I+INM)
      MGAP=IW(I+IWK)-IW(I+IGP)
      MWRK=IW(I+NJW)-IW(I+IWK)+1
      WRITE(NPR,1003) J,CHAINT(IW(I+IAR+1)),CHAINT(IW(I+IAR+2)),
     &                IW(I+NJW),NSYST,NAMES,MNAM,IW(I+NDN),
     &                MGAP,MWRK,IW(I+NDW)
   10 CONTINUE
      IF(IMAX.EQ.1) GOTO 100
C
      WRITE(NPR,1004)
      DO J=1,NARR
        I=IARR(J)-IARR(1)
        WRITE(NPR,1005) J,CHAINT(IW(I+IAR+1)),CHAINT(IW(I+IAR+2)),
     &                  (IW(I+IRC+K),K=1,6),IW(I+NGN),IW(I+NGW),
     &                  (IW(I+NFH+K),K=1,10)
      ENDDO
C
      WRITE(NPR,1007)
     +'Return code (1) = insufficient space to create new bank'
      WRITE(NPR,1007)
     +'            (2) = insuff. space to increase bank length'
      WRITE(NPR,1007)
     +'            (3) = named bank not found (BKTOW, BKFRW)'
      WRITE(NPR,1007)
     +'            (4) = read error'
      WRITE(NPR,1007)
     +'            (5) = formal error in record structure'
      WRITE(NPR,1007)
     +'            (6) = insuff. space for record input'
      WRITE(NPR,1007) ' '
      WRITE(NPR,1007) 'The histogram (in form of counts) shows usage of
     + the BOS commons in units of 10 percent.'
      WRITE(NPR,1007) 'Interesting is especially the last nonzero number
     +. If for example the last nonzero number is before the 70,'
      WRITE(NPR,1007) 'then the program never used more than 70 percent
     +of the total BOS common.'
C
      CALL BOSIO
      CALL BOSRPT
  100 RETURN
 1001 FORMAT(/' BOS++BOSTA+  Status of BOS',I4,' min',F7.3,' sec after',
     1   ' initialization  (',F7.1,' sec left)')
 1002 FORMAT(/'      BOS-ARRAY  LENGTH  NUMBER OF WORDS IN AREAS...'/
     1   25X,'SYSTEM   INDEX NAMED-B   (DROP)    GAP  WORK-B   (DROP)')
 1003 FORMAT(1X,I3,3X,2A4,8I8)
 1004 FORMAT(/'      BOS-array      return code statistic...',22X,
     1   'garb.collect.',3X,
     2   'histogram  of  used  space  in  10 percent bins'/,21X,
     3   '(1)',5X,'(2)',5X,'(3)',5X,'(4)',5X,'(5)',5X,'(6)',6X,'N',7X,
     4   'W',4X,'10   20   30   40   50   60   70   80   90  100')
 1005 FORMAT(1X,I3,3X,2A4,8I8,10(1X,I4))
 1006 FORMAT(/' +++++++++++  B O S - C L A S  version',F7.4,'/')
 1007 FORMAT(1X,A)
      END
