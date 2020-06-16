c  from patch gcdes in file /usr/site3/cern/hp700_ux90/94a/src/car/geant321.car on Thu Nov 10 11:00:38 EST 1994
c  this is seq: GCNMEC
      PARAMETER(NMECA=45,IBLOWN=23)
      CHARACTER*4 CHNMEC(NMECA)
*
* *** Two blank spaces for FISF and CAPF which do not exist
*
* *** The low-neutron cross-sections have to be at the end for the
* *** logics of the routine to work, and they have to start at
* *** IBLOWN
      DATA CHNMEC /
     +     'HADF',  'INEF',  'ELAF',  'NULL',  'NULL',
     +     'HADG',  'INEG',  'ELAG',  'FISG',  'CAPG',
     +     'LOSS',  'PHOT',  'ANNI',  'COMP',  'BREM',
     +     'PAIR',  'DRAY',  'PFIS',  'RAYL',  'MUNU',
     +     'RANG',  'STEP',  'LOWN',  'NABS',  'NELA',
     +     'NINE',  'NINC',  'N,2N',  'N,3N',  'N,NA',
     +     'N2NA',  'N,NP',  'NFIS',  'N,GA',  'N,PR',
     +     'N,DE',  'N,TR',  'N3HE',  'N,AL',  'N,2A',
     +     'N,3A',  'N,2P',  'N,PA',  'NT2A',  'ND2A'/
*
