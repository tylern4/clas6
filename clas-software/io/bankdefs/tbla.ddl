!----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TBLA  ! create write display delete ! Time Based tracking LAyer bank
!                                             (output of Time Based Tracking)
!  record_no = sector# 
!
!  34 rows (=number of DC layers) for every track in this sector
!      ['trk_plane' counts 46 planes: Origin(1plane),Start_Cnt.(2planes),
!                           DC(36planes),CC(1plane),SC(4planes),EC(2planes)]
!    4 Layers in Superlayer 1 (Region 1 Stereo)  [=trk_planes  4-7 ]
!    6 Layers in Superlayer 2 (Region 1 Axial)   [=trk_planes 10-15]
!    6 Layers in Superlayer 3 (Region 2 Axial)   [=trk_planes 16-21]
!    6 Layers in Superlayer 4 (Region 2 Stereo)  [=trk_planes 22-27]
!    6 Layers in Superlayer 5 (Region 3 Axial)   [=trk_planes 28-33]
!    6 Layers in Superlayer 6 (Region 3 Stereo)  [=trk_planes 34-39]
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + Trk_plane_number
  2  x         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  3  y         F  -1000.   1000.  ! y coord [cm]  for track in this plane
  4  z         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  5  Bx        F   -100.    100.  ! B-field in x [kG] at coord.{x,y,z}
  6  By        F   -100.    100.  ! B-field in y [kG] at coord.{x,y,z}
  7  Bz        F   -100.    100.  ! B-field in z [kG] at coord.{x,y,z}
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
  9  dc1       I      0    99999  ! Pointer to DC1 bank
 10  stat      I      1      100  ! Status of the hit
 11  wire      I      1      192  ! Wire number 
 12  dtime     F   -100.    5000. ! drift time  [ns]
 13  alpha     F    -40.      40. ! track angle (relative to R of SL) [deg]
 14  wlen      F      0.     200. ! Wire length (hit pos. to preamp)  [cm]
 15  sgdoca    F   0.001       5. ! sigma DOCA  [cm]
 16  fitdoca   F    -10.      10. ! Fitted DOCA [cm]
 17  calcdoca  F    -10.      10. ! calculated DOCA (via dtime)  [cm]
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!
