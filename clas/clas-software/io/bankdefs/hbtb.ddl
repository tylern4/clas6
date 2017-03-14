!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBTB  ! create write display delete ! Hit Based to Time Based tracking
!                                             Intermediate bank
! record_no = Sector#
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
! For Resolving the LeftRight Ambiguity all Segments in Clusters that matched
!   to the track are tested, therefore we have to keep these Segments if we
!   want to separate Hit & Time Based Tracking
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk       I       1     20    ! Track Candidate 
  2  sly       I       1      6    ! Superlayer
  3  icl       I       1     10    ! Cluster# that matched to the track
  4  isg       I       1     20    ! Segment# in this cluster
  5  id1       I       1  10000    ! Ptr. to DC1 entry  for 1.Layer in SL
  6  id2       I       1  10000    ! Ptr. to DC1 entry  for 2.Layer in SL
  7  id3       I       1  10000    ! Ptr. to DC1 entry  for 3.Layer in SL
  8  id4       I       1  10000    ! Ptr. to DC1 entry  for 4.Layer in SL
  9  id5       I       1  10000    ! Ptr. to DC1 entry  for 5.Layer in SL
 10  id6       I       1  10000    ! Ptr. to DC1 entry  for 6.Layer in SL
!
 END TABLE

