c  from patch gcdes in file /usr/site3/cern/hp700_ux90/94a/src/car/geant321.car on Thu Nov 10 11:00:38 EST 1994
c  this is seq: PAWC
      INTEGER NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE,LLMAIN,IQQ,LQQ
      REAL WWS,QQ
      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE(5),
     +              LLMAIN, WWS(9989)
      DIMENSION IQQ(2),QQ(2),LQQ(8000)
      EQUIVALENCE (QQ(1),IQQ(1),LQQ(9)),(LQQ(1),LLMAIN)
C
