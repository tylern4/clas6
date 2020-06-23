CDECK  ID>, WGARB.
      SUBROUTINE WGARB(JW)
c
c_begin_doc
c  RCS ID string
c  $Id: wgarb.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $
c
c  Purpose:
c  --------
c     GARBAGE COLLECTION FOR WORK BANKS
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
      INTEGER JW(*)
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
     1'$Id: wgarb.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='WGARB.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NUS,IH,ICOP,ISTP,ID,NSTP,N,J,I
      INTEGER INDN(200)
      EQUIVALENCE (INTA(1),INDN(1))
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WGARB')
      JW(1)=0
      JW(2)=0
      IF(JW(NDW).EQ.0) GOTO 100
C
*     STATISTIC ON GARBAGE COLLECTION
C
      JW(NGW)=JW(NGW)+1
      NUS=JW(NJW)-JW(IWK)+JW(IGP)-JW(NDW)
      IH=10.0*FLOAT(NUS)/FLOAT(JW(NJW))+1.0
      IH=MIN(10,MAX(1,IH))
      JW(NFH+IH)=JW(NFH+IH)+1
C
      ICOP=JW(IGW)
      ISTP=JW(IGW)
C
   10 ID  =JW( IWK) ! first index of work bank area
      NSTP=0
      N   =0
C
C     STORE INDEX AND WORD COUNT FOR LATER COPY OF WORK BANK
C
   20 IF(JW(ID+(NHW-1)).GE.0) THEN
C
C        WORK BANK IS DROPPED
C
         IF(N.GT.0.AND.ID.EQ.INDN(N-1)+INDN(N)) THEN
            INDN(N)=INDN(N)+JW(ID+(NHW-3))
         ELSE
            IF(N.GE.200) THEN
               NSTP=INDN(N-1)+1
               N   =0
            END IF
            N=N+2
            INDN(N-1)=ID
            INDN(N  )=JW(ID+(NHW-3))
         END IF
      END IF
      ID=ID+JW(ID+(NHW-3))
      IF(ID.LT.ISTP) GOTO 20
C
C     COPY BANKS, TO REMOVE GAPS
C
      DO 30 J=1,N,2
         I=N-J
         ICOP=ICOP-INDN(I+1)
         CALL UCOPY2(JW(INDN(I)),JW(ICOP),INDN(I+1))
   30 CONTINUE
      ISTP=NSTP
      IF(ISTP.NE.0) GOTO 10
C
C     UPDATE INDICES
C
      JW(IWK)=ICOP  ! first index of work bank area
      JW(NDW)=0     ! the number of dropped words of work banks
      ID=ICOP+(NHW-1)
   40 IF(ID.LT.JW(IGW)) THEN
         JW(JW(ID-1))=ID
         ID=ID+JW(ID-2)
         GOTO 40
      END IF
      JW(IGW)=0
C exit
  100 RETURN
      END
