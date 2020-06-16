!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  LCPB    ! create write display delete ! LAC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScHt     I     0    10      ! 100*sector+Hit_ID in EC1R
  2  Etot     F     0.   20.     ! Reconstructed total energy
  3  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  4  Path     F     0.  600.     ! Path lenght from target
  5  X        F  -999.  999.     ! x coordinate of the hit
  6  Y        F  -999.  999.     ! y coordinate of the hit
  7  Z        F  -999.  999.     ! z coordinate of the hit
  8  Chi2LC   F     0.  100.     ! Quality measure of geometrical matching
  9  Status   I     0   0xFFFF   ! Status word
 10  Ein      F     0.   20.     ! Reconstructed energy in the inner part
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
