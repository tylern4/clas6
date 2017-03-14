!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECMT       ! create write display delete ! Mean EC pretrigger thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   in_high      I     0  100000  ! inner high threshold (mV)
  2   out_high     I     0  100000  ! outer high threshold (mV)
  3   total_high   I     0  100000  ! total high threshold (mV)
  4   in_lo        I     0  100000  ! inner lo threshold (mV)
  5   out_lo       I     0  100000  ! outer lo threshold (mV)
  6   total_lo     I     0  100000  ! total lo threshold (mV)

!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
