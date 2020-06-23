c  from patch gcdes in file /usr/site3/cern/hp700_ux90/94a/src/car/geant321.car on Thu Nov 10 11:00:38 EST 1994
c  this is seq: GCJUMP
      INTEGER       JUDCAY, JUDIGI, JUDTIM, JUFLD , JUHADR, JUIGET,
     +              JUINME, JUINTI, JUKINE, JUNEAR, JUOUT , JUPHAD,
     +              JUSKIP, JUSTEP, JUSWIM, JUTRAK, JUTREV, JUVIEW,
     +              JUPARA
      INTEGER       JMPADR, MAXJMP
*
      PARAMETER    (MAXJMP=30)
      COMMON/GCJUMP/JUDCAY, JUDIGI, JUDTIM, JUFLD , JUHADR, JUIGET,
     +              JUINME, JUINTI, JUKINE, JUNEAR, JUOUT , JUPHAD,
     +              JUSKIP, JUSTEP, JUSWIM, JUTRAK, JUTREV, JUVIEW,
     +              JUPARA
      DIMENSION     JMPADR(MAXJMP)
      EQUIVALENCE  (JMPADR(1), JUDCAY)
*
