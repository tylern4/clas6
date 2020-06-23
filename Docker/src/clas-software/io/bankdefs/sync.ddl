!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SYNC     B16   ! create write display delete ! Physics sync event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     0      25  ! slot number
  2  TDCL     I     0   65536  ! count of missing responses
  3  ADCL     I     0   65536  ! count of extra buffers
!
 END TABLE

