CDECK  ID>, BDCACH.
      SUBROUTINE BDCACH(NWORDS)
c
c_begin_doc
c  RCS ID string
c  $Id: bdcach.F,v 1.1.1.1 1997/03/28 20:49:29 marki Exp $
c
c  Purpose:
c  --------
c     drop some cache banks
c     NWORDS < 0   at least NWORDS words
c                  called in NBANK ... in case of no space
c     NWORDS = 0   10 percent of space, at least 1 bank
c                  called in WGARB
c     NWORDS > 0   initialize (called in BDABF), also user call to
c                  define space NWORDS for direct access buffer
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
cboy+SEQ, DACOM, COCACH.    temporarily ???
C------------------------------------------------------
      INTEGER NCACHM,NCACHE,NWCACH,NACACH,ICHLOK,IDC,ICL,IRU
      PARAMETER    (NCACHM=400)
      COMMON/COCACH/NCACHE,NWCACH,NACACH,ICHLOK,
     +              IDC(NCACHM),ICL(NCACHM),IRU(NCACHM)
*                   index       clock       rec/unit
      SAVE  /COCACH/
*     NCACHM = maximum number of cache records = work banks
*     NCACHE = actual number of cache records
*     NWCACH = allowed number of words for cache records
*     NACACH = actual number of words for cache records
C------------------------------------------------------
C COMMON/DACOM FOR DIRECT ACCESS SUBROUTINES
      INTEGER LUNDA,NWRDA,NPRDA,IUB,IDR,IDD,NRR,NRD,NDD,NADA,NRDA,NDDA,
     &        NDDR,INDA,NHDA,NDTD,NDTC,NDTR,IDRR,IDDR,INW,NIW,NEX,INA,
     &        JRLO,JRHI,NRLO,NRHI,NI,NIR
      COMMON/DACOM/LUNDA,NWRDA,NPRDA,
     1             IUB,  IDR,  IDD,  NRR,  NRD,  NDD,
     1             NADA, NRDA, NDDA, NDDR, INDA, NHDA,
     2             NDTD, NDTC, NDTR, IDRR, IDDR,
     3            INW,NIW,NEX,INA,JRLO,JRHI,NRLO,NRHI,
     4             NI,NIR(200)
      SAVE  /DACOM/
C------------------------------------------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER NWORDS
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
     1'$Id: bdcach.F,v 1.1.1.1 1997/03/28 20:49:29 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BDCACH.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,NACLIM,IOLD,IL,IR
      LOGICAL START
      DATA START/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(START) THEN
         START=.FALSE.
         NCACHE=0
         NACACH=0
         ICHLOK=0
         DO 10 I=1,NCACHM
   10    IDC(I)=0
         IF(NWORDS.GT.1) THEN
*           user defined cache space
            NWCACH=NWORDS
         ELSE
*           default is 10 percent of BCS
            IF(IW(30).EQ.12345) THEN
               NWCACH=IW(12)/10
            ELSE
               NWCACH=1000
            END IF
         END IF
      END IF
*     clock
      ICHLOK=ICHLOK+1
*     set limit for cache region
      IF(NWORDS.GT.1) THEN
         NWCACH=NWORDS
      END IF
      IF(NWORDS.GT.0) THEN
*        initialization done
         NACLIM=NWCACH
         IF(NACACH.LE.NACLIM) GOTO 100
      ELSE IF(NWORDS.EQ.0) THEN
*        reduce by at least 10 percent
         NACLIM=0.9*FLOAT(NACACH)
      ELSE
*        reduce by NWORDS
         NACLIM=NACACH+NWORDS
      END IF
*     find oldest record bank
   20 IOLD=ICHLOK
      DO 30 IL=1,NCACHE
      IF(ICL(IL).LT.IOLD) THEN
         IOLD=ICL(IL)
         IR  =IL
      END IF
   30 CONTINUE
      IF(IOLD.NE.ICHLOK) THEN
*        drop old bank
         NACACH=NACACH-IW(IDC(IR)-2)
*        exchange with last (unless last)
         IF(IR.NE.NCACHE) THEN
            CALL WSWAP(IW,IDC(IR),IDC(NCACHE))
            ICL(IR)=ICL(NCACHE)
            IRU(IR)=IRU(NCACHE)
         END IF
         CALL WDROP(IW,IDC(NCACHE))
         NCACHE=NCACHE-1
      ELSE
*        no bank to drop
         GOTO 100
      END IF
      IF(NACACH.GT.NACLIM) GOTO 20
  100 RETURN
      END
