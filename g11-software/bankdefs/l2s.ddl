!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  L2S       B16  ! create write display delete ! Level 2 report bank
!
! id = 1 - TL1 - reported TDC for Level 1
! id = 2 - L2S1 - reported 2 segments out of 4 from sector 1
! id = 3 - L2S2 - reported 2 segments out of 4 from sector 2
! id = 4 - L2S3 - reported 2 segments out of 4 from sector 3
! id = 5 - L2S4 - reported 2 segments out of 4 from sector 4
! id = 6 - L2S5 - reported 2 segments out of 4 from sector 5
! id = 7 - L2S6 - reported 2 segments out of 4 from sector 6
! id = 8 - L2OR - Level 2 final report (OR from all sectors)
! id = 9 - L2NOR - Level 2 fault
!   Attributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID      I   256    9408  ! ID+256*hit#
  2  TDC     I     0   65536  ! TDC value
!

 END TABLE
!
