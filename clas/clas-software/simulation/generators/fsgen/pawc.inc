C----------------------------------------------------------------------
C-
C-   Created  6-MAY-1996   Stepan Stepanyan, CEBAF
C-
C----------------------------------------------------------------------
C
C ****  COMMON PAWC  Paw program common block
C
      INTEGER LIMHST
      INTEGER HMEMOR
      PARAMETER (LIMHST=800000)
      COMMON /PAWC/ HMEMOR(LIMHST)
