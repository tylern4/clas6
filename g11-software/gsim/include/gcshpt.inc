c  from patch gcdes in file /usr/site3/cern/hp700_ux90/94a/src/car/geant321.car on Thu Nov 10 11:00:38 EST 1994
c  this is seq: GCSHPT
C  Shells are numbered from 1 to 24.
C  Shells used:
C               K,L1,L2,L3,M1,M2,M3,M4,M5
C               N1,N2,N3,N4,N5,N6,N7,
C               O1,O2,O3,O4,O5,P1,P2,P3
C   VARIABLES:
C     NSHLST - value of Z for which the shells starts to be present
C     N1ST   - pointer to K shell of a given Z (in ESHELL array)
C     NSHLLS - Number of used shells for a given Z
C     ESHELL - Shells potentials in eV !!!
      INTEGER LENGTH,MAXSHL
      PARAMETER (LENGTH=  1409)
      PARAMETER (MAXSHL=24)
      INTEGER NSHLST,N1ST,NSHLLS
      REAL ESHELL
      DIMENSION NSHLST(MAXSHL),N1ST(MAXELZ),NSHLLS(MAXELZ)
      DIMENSION ESHELL(LENGTH)
      COMMON /GCSHPT/NSHLST,N1ST,NSHLLS,ESHELL
C
