!********************************************************************
!
!                   Celeg MC BOS banks
!                   =======================
!
!********************************************************************
!* ACTION = CREATE implies BKFMT will be called
!* ACTION = WRITE implies BLIST('+bname') will be called
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEAD    ! write  create   ! Bank HEAD
!
!COL ATT-name FMT Min    Max   !Comments
   1 VERSION  I    0      2    ! Version Number
   2 NRUN     I    1      1000 ! Run Number (monotonically increasing)
   3 NEVENT   I    1    100000 ! Event Number (starting with 1 at run begin)
   4 TIME     I 100  100000000 ! Event Time (UNIX time = seconds as of January 1,1970)
   5 TYPE     I    0      1000 ! Event Type (Defined by on-line system or MC run:
                               !            > 0 Triggered (Physics) Events
                               !            = 0 Control Records
                               !            < 0 Monte Carlo Events:
                               !                 = -1 SDA
                               !                 = -2 GEANT
                               !                 = -3 ClasSim
                               !                 = -4 Celeg
   6 ROC      I    0      20   ! 32 bit readout controller status
   7 CLASS    I    1      20   ! Event Classification from event builder
                               !     1-15 Physics Events
                               !       16 Sync Event
                               !       17 Prestart Event
                               !       18 Go Event
                               !       19 Pause Event
                               !       20 End Event
   8 WEIGHT   I    0      1000 ! Prescale factor for this Event Class (Trigger Type)=
                               ! =Number of triggers to get an event
!
 END BANK
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCTK      ! write  create  ! GSIM Monte Carlo track bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  cx       F    -1.   1.    ! x dir cosine at track origin
  2  cy       F    -1.   1.    ! y dir cosine
  3  cz       F    -1.   1.    ! z dir cosine
  4  pmom     F     0.   20.   ! momentum
  5  mass     F     0.   10.   ! mass
  6  charge   F    -1.    1.   ! charge
  7  id       I  -5000   5000  ! track Particle Data Group id
  8  flag     I     0  0xFFFF  ! track flag
  9  beg_vtx  I     0   65536  ! beginning vertex number 
 10  end_vtx  I     0   65536  ! ending vertex number
 11  parent   I     0   65536  ! parent track
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
 TABLE  MCVX     ! write  create ! GSIM Monte Carlo vertex parameters
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x       F   -1000.  2000.    ! x of vertex
  2  y       F   -1000.  2000.    ! y
  3  z       F   -1000.  2000.    ! z
  4  tof     F   -1000.  2000.    ! 
  5  flag    I       0   65536    ! vertex flag
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGR  ! create write ! Tagger result bank (AL-LYM-FR 9/29/1997 --- 
FYDW)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ERG       F    0.  10.     ! Energy of the photon in GeV
  2  TTAG      F    -20.  200.  ! Time of the photon has reconstructed in the Tagger
  3  TPHO      F    -20.  200.  ! Time of the photon after RF correction
  4  STAT      I    0   4096    ! Status ( 7 or 15 are Good) other values have problems (s
ee tag_process_TAGR.F) 
  5  T_id      I    1   121     ! T counter Id
  6  E_id      I    1   767     ! E counter Id
!
 END TABLE
!
END$








