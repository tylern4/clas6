!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EC01     ! create write display delete ! Scintillation counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      48  ! the address of the hit detector element
  2  time    F     0   100000  ! time for left paddle(ns) 
  3  energy    F     0   65536  ! energy in left paddle(MeV) 
  
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
