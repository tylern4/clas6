!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SC1      ! create write display delete ! Scintillation counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       	I     1      48  ! the address of the hit detector element
  2  time_l     F     0   100000  ! time for left paddle(ns) 
  3  energy_l   F     0   65536  ! energy in left paddle(MeV) 
  4  time_r     F     0   65536  ! time for right paddle(ns) 
  5  energy_r   F     0   65536  ! energy in right paddle(MeV) 
  6  dtime_l    F   0  65536  ! uncertainty in time for left paddle(ns) 
  7  denergy_l  F   0  65536  ! uncertainty in energy in left paddle(MeV) 
  8  dtime_r    F   0  65536  ! uncertainty in time for right paddle(ns) 
  9  denergy_r  F   0  65536  ! uncertainty in energy in right paddle(MeV) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
