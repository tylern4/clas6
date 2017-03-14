!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEVT      ! create write display delete !Reconstructed event, Head bank
!
!COL ATT-name FMT Min    Max   !Comments
   1 ESTATUS  I   -1        1  ! Event Statuse after reconstruction
   2 NPART    I    0       10  ! Number of final reconstructed particles
   3 GPART    I    0       10  ! Number of geometrically reconstructed particles
   4 WEIGHT   I    0      1000 ! Prescale factor for this Event Class
   5 TRIGGER  I    0      1000 ! Trigger Type
   6 FC       R    0.      500.! Faraday Cup (nA)
   7 PTIME    I 100  100000000 ! Event Processing Time (UNIX time = seconds)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EVNT    ! create write display delete ! RECSIS reconstructed event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Ipart    I     0    10      ! Bank row number 
  2  ID       I  -5000   5000    ! Particle Data Group id
  3  Pmom     F     0.   20.     ! momentum
  4  Mass     F     0.   10.     ! mass
  5  Charge   I    -1     1      ! charge
  6  Betta    F     0.    1.     ! Particle velocity in the units of c
  7  Cx       F    -1.    1.     ! x dir cosine at track origin
  8  cy       F    -1.    1.     ! y dir cosine at track origin
  9  cz       F    -1.    1.     ! z dir cosine at track origin
 10  X        F  -100.  100.     ! X coordinate of vertex (cm)
 11  Y        F  -100.  100.     ! Y coordinate of vertex (cm)
 12  Z        F  -100.  100.     ! Z coordinate of vertex (cm)
 13  DCstat   I     0    50      ! Status of DC (=0 if DC is not involved) 
 14  CCstat   I     0    50      ! Status of CC (=0 if CC is not involved) 
 15  SCstat   I     0    50      ! Status of SC (=0 if SC is not involved)  
 16  ECstat   I     0    50      ! Status of EC (=0 if EC is not involved) 
 17  Chi2DC   F     0.   20.     ! Chisquare of track fitting
 18  Chi2CC   F     0.   10.     ! Chisquare of geometrical matching
 19  Chi2SC   F     0.   10.     ! Chisquare of geometrical matching
 20  Chi2EC   F     0.   10.     ! Chisquare of geometrical matching
 21  Status   I     0   0xFFFF   ! Status word (=0 out of time particle)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCPB    ! create write display delete ! DC tracks involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Itrk     I     0    10      ! Number of rows and Bank row number 
  2  Ptrk     F     0.   20.     ! Reconstructed track momentum
  3  Charge   I    -1     1      ! charge
  4  Cx       F    -1.    1.     ! x dir cosine at track origin
  5  cy       F    -1.    1.     ! y dir cosine at track origin
  6  cz       F    -1.    1.     ! z dir cosine at track origin
  7  X        F  -100.  100.     ! X coordinate of vertex (cm)
  8  Y        F  -100.  100.     ! Y coordinate of vertex (cm)
  9  Z        F  -100.  100.     ! Z coordinate of vertex (cm)
 10  Chi2     F     0.   20.     ! Chisquare of track fitting
 11  Sector   I     0     6      ! DC track sector 
 12  iDCtrk   I     0    30      ! DC track ID
 13  Status   I     0   0xFFFF   ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Ihit     I     0    10      ! Bank row number 
  2  ID       I  -5000   5000    ! Particle Data Group id
  3  Etot     F     0.   20.     ! Reconstructed total energy
  4  Einout   F     0.   20.     ! Inner and Outer energy difference
  5  Charge   I    -1     1      ! Charge
  6  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  7  Path     F     0.  600.     ! Path lenght from target
  8  Cx       F    -1.    1.     ! x dir cosine at track origin
  9  cy       F    -1.    1.     ! y dir cosine at track origin
 10  cz       F    -1.    1.     ! z dir cosine at track origin
 11  Sector   I     0     6      ! EC hit sector 
 12  iWhole   I     0    30      ! EC hit ID for the layer WHOLE 
 13  iInner   I     0    30      ! EC hit ID for the layer INNER   
 14  iOuter   I     0    30      ! EC hit ID for the layer OUTER  
 15  Status   I     0   0xFFFF   ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Ihit     I     0    10      ! Bank row number 
  2  ID       I  -5000   5000    ! Particle Data Group id
  3  Edep     F     0.    1.     ! Deposited energy (dE/dX)
  4  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  5  Path     F     0.  600.     ! Path lenght from target
  6  Sector   I     0     6      ! SC hit sector 
  7  iSChit   I     0    30      ! SC hit ID 
  8  Status   I     0   0xFFFF   ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCPB    ! create write display delete ! CC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Ihit     I     0    10      ! Bank row number 
  3  Nphe     F     0. 1000.     ! Number of photo-electrons
  4  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  5  Path     F     0.  600.     ! Path lenght from target
  6  Sector   I     0     6      ! CC hit sector 
  7  iCChit   I     0    30      ! CC hit ID 
  8  Status   I     0   0xFFFF   ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   UNUS      ! create write display delete ! Unused hits/tracks bank
!
!COL ATT-name FMT Min    Max   !Comments
   1 NDCUN    I    0     20    ! Number of unused DC tracks
   2 IDCUN    I    0     20    ! Trek ID  
   1 NSCUN    I    0     20    ! Number of unused SC hits
   2 ISCUN    I    0     20    ! SC hit ID
   1 NCCUN    I    0     20    ! Number of unused CC hits
   2 ICCUN    I    0     20    ! CC hit ID
   1 NECUN    I    0     20    ! Number of unused EC hits
   2 IECUN    I    0     20    ! EC hit ID
!
 END TABLE
!
