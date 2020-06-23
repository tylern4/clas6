CDECK  ID>, BOSIO.
      SUBROUTINE BOSIO
c
c_begin_doc
c  RCS ID string
c  $Id: bosio.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $
c
c  Purpose:
c  --------
c
c     BANK +BUF LUN
c                                   'FORT'      'EPIO'       'TEXT'
c      1   packed information  see    0+          10+          20+
c                                   + 0 (NEUTRAL) + 1 (READ) + 2 (WRITE)
c      2   blocksize                 WORDS      16BIT WORDS   BYTES
c      3   NR OF IO ERRORS
c      4   nr of blocks (read/write blocks)/ last record
c      5   nr of segments within record
c      6   nr of BOS records
c      7   MAX NR OF WORDS/EVENT
c      8   MBYTES
c      9   IEFMT-FLAG record pointer
c     10   pointer within record (if zero, no record present)
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
     1'$Id: bosio.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSIO.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NAMIND,I,IPA(6),IBF,IPFL,IOAC,IOMD,IOST
c      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*24 TMODE
      CHARACTER*9  TBUFF
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(LUP).EQ.0) GOTO 100
      IBF=NAMIND('+BUF')+1
      IF(IW(IBF-1).EQ.0) GOTO 100
*     loop on all non-DAF files
      IPFL=0
   10 IBF=IW(IBF-1)
      IF(IBF.EQ.0) GOTO 20
C------ KEEP, UNPACKIO ----------------------------------------------
      IOAC=MOD(IW(IBF+1),8)
      IOMD=MOD(IW(IBF+1)/8,8)
      IOST=MOD(IW(IBF+1)/64,8)
C--------------------------------------------------------------------
 
      IF(IOMD.EQ.7) GOTO 10
      IF(IPFL.EQ.0) WRITE(IW(LUP),1001)
      IPFL=1
      TMODE=' '
      IF(IOMD.EQ.0) THEN
*        fortran
         TMODE='FORT'
         TBUFF='WORDS'
         IF(IOST.EQ.1) THEN
            TMODE(10:14)='read '
            IF(IW(IBF+11).EQ.0) TMODE(16:24)='endfile'
         END IF
         IF(IOST.EQ.2) THEN
            TMODE(10:14)='write'
            IF(IW(IBF+11).EQ.0) TMODE(16:24)='buff out'
         END IF
      END IF
      IF(IOMD.EQ.1) THEN
*        epio
         TMODE='EPIO'
         TBUFF='16bit wds'
      END IF
      IF(IOMD.EQ.2) THEN
*        text
         TMODE='TEXT'
         TBUFF='char/line'
      END IF
      IF(IOST.EQ.1) TMODE(10:14)='read '
      IF(IOST.EQ.2) TMODE(10:14)='write'
      IF(IOST.EQ.3) TMODE(16:24)='rewind'
      WRITE(IW(LUP),1002) IW(IBF-2),TMODE,IW(IBF+2),TBUFF,
     1   (IW(IBF+I),I=3,7),RW(IBF+8)
      GOTO 10
   20 WRITE(IW(LUP),1002)
*     DAF files
      IPFL=0
      IBF=NAMIND('+BUF')+1
   30 IBF=IW(IBF-1)
      IF(IBF.EQ.0) GOTO 40
C------ KEEP, UNPACKIO ----------------------------------------------
      IOAC=MOD(IW(IBF+1),8)
      IOMD=MOD(IW(IBF+1)/8,8)
      IOST=MOD(IW(IBF+1)/64,8)
C--------------------------------------------------------------------
 
      IF(IOMD.NE.7) GOTO 30
      IF(IPFL.EQ.0) WRITE(IW(LUP),1003)
      IPFL=1
      IPA(1)=IW(IBF+11)
      IPA(2)=IW(IBF+12)
      IPA(3)=3
      IPA(4)=IW(IBF+12)-IW(IBF+14)+1
      IPA(5)=IW(IBF+13)-3
      IPA(6)=IW(IBF+14)-IW(IBF+13)-1
      WRITE(IW(LUP),1004) IW(IBF-2),IPA,IW(IBF+16),IW(IBF+17),
     + (IW(IBF+I),I=3,6)
      GOTO 30
   40 WRITE(IW(LUP),1004)
  100 RETURN
 1001 FORMAT(/' BOS++BOSIO+  Status of input/output'/
     1   ' unit',3X,'          ',18X,'buffer-size',7X,
     1   '  errors    blocks  segments  BOS-recs ',
     2   ' max wrds    Mbytes')
 1002 FORMAT(1X,I4,3X,A24,I10,1X,A9,5I10,F10.3)
 1003 FORMAT(/' unit',3X,'direct access',13X,'number of records',14X,
     1   'date',14X,'IO-opeartions'/
     2   26X,' total   sys   dir  data  free',4X,'instal   modif',
     3   6X,'read',11X,'write')
 1004 FORMAT(1X,I4,3X,'recl=',I5,' words',2X,5I6,2X,2I8,2X,
     + I8,' +',I6,I8,' +',I6)
      END
