!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPO        ! create write display delete ! Forward EC hit pointerw
!                                                   to the strips.
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  UID1       I     1   999999  ! Pointer to the U-strips 1-18 for hit
  2  UID2       I     1   999999  ! Pointer to the U-strips 19-3 for hit
  3  VID1       I     1   999999  ! Pointer to the V-strips 1-18 for hit
  4  VID2       I     1   999999  ! Pointer to the V-strips 19-3 for hit
  5  WID1       I     1   999999  ! Pointer to the W-strips 1-18 for hit 
  6  WID2       I     1   999999  ! Pointer to the W-strips 19-36 for hit 
  7  SLH        I     1   999999  ! Sector*1000+Layer*100+Hit
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
