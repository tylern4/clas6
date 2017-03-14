!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STPB    ! create write display delete ! ST hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  SThid    I     0    10      ! 100*sector+Hit_ID  
  2  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  3  Path     F     0.  600.     ! Path lenght from target
  4  charge   I    -1.    1.     ! track number from STR
  5  Status   I     0   0xFFFF   ! Status from STR
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
