C----------------------------------------------------------------------
C-
C-   Created  6-MAY-1996   Stepan Stepanyan, CEBAF
C-
C----------------------------------------------------------------------
C
C ****  COMMON LUDAT1  Lund program common block
C
      INTEGER mstu
      INTEGER mstj
      REAL    paru
      REAL    parj
      COMMON /ludat1/ mstu(200),paru(200),mstj(200),parj(200)
C
C ****  COMMON LUDAT2  Lund program common block
C
      INTEGER kchg
      REAL    pmas
      REAL    parf
      REAL    vckm
      COMMON /ludat2/ kchg(500,3),pmas(500,4),parf(2000),vckm(4,4)
C
C ****  COMMON LUDAT3  Lund program common block
C
      INTEGER mdcy 
      INTEGER mdme 
      INTEGER kfdp
      REAL    brat
      COMMON /ludat3/ mdcy(500,3),mdme(2000,2),brat(2000),kfdp(2000,5)
C
C ****  COMMON LUDAT4  Lund program common block
C
      CHARACTER   chaf*8
      COMMON /ludat4/ chaf(500)
