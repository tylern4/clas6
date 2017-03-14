!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TPCH      B32   ! create write display delete ! BONUS eventheader bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  NEVENT   I     0  100000000 ! Event Number (starting with 1)
  2  TIME     I     0  100000000 ! Time stamp
  3  NTPC     I     0   65536    ! Number of TPC rows
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
