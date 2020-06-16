!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScPdHt   I     0    10      ! 10000*sector+100*SC_PD_ID+Hit_ID in SCR 
  2  Edep     F     0.    1.     ! Deposited energy (dE/dX)
  3  Time     F    -5.  150.     ! measured time 
  4  Path     F     0.  600.     ! Path lenght from target
  5  Chi2SC   F     0.   10.     ! Quality measure of geometrical matching
  6  Status   I     0   0xFFFF   ! Status word (not defined yet)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
