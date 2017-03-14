c  from patch gcdes in file /usr/site3/cern/hp700_ux90/94a/src/car/geant321.car on Thu Nov 10 11:00:38 EST 1994
c  this is seq: GCPHRD
C  GRATE - radiative modes' rates
      INTEGER KSHLS,L1SHLS,L2SHLS,L3SHLS,ISHLS,ISHLUS,ISHLTR
      REAL GPHRAT
      PARAMETER (KSHLS=6)
      PARAMETER (L1SHLS=8)
      PARAMETER (L2SHLS=7)
      PARAMETER (L3SHLS=8)
      PARAMETER (ISHLS=29)
      COMMON / GCPHRD / GPHRAT(ISHLS,MAXELZ),ISHLUS(24,4),ISHLTR(ISHLS)
C
