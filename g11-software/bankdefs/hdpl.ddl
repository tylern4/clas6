!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HDPL  ! create write display delete ! Hbt Detector PLane bank
!                                           ! output of Hit Based Tracking
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE SECTOR COORDINATE SYSTEM.
! 
! Record_no = sector#
!
! 10 rows per track in a sector  [i.e. for 2 tracks in one sector: 20 rows]:
!        1 plane for 'Vertex position (#1): closest distance to beamline
!        2 planes for Start Counter     (#2 , #3)
!        1 plane for Cerenkov Counter   (#40)
!        4 planes for Scint.Counters    (#41(=SCplane 1), #42(=SCplane 2),
!                                       (#43(=SCplanes 3&4),#44(=SCplanes5&6))
!        2 planes for Elmag.Calorimeter (#45(=Forw.EC), #46(=LA EC))
!        (same structure as TDPL (for Time Based Tracking))
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + plane_number
  2  x         F  -1000.   1000.  ! vector3_t pos;
  3  y         F  -1000.   1000.  !  (x, y, z coord. for track in on the plane)
  4  z         F  -1000.   1000.  ! 
  5  cx        F     -1.      1.  ! vector3_t dir;
  6  cy        F     -1.      1.  !  direction cosines (x,y,z) for track at coord.{x,y,z}
  7  cz        F     -1.      1.  ! 
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!

