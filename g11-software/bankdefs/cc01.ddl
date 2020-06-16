!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CC01     ! create write display delete ! Cerenkov Counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  id       I     0    36     ! the address of the hit detector element
  2  time     F     0   100000  ! time(ns) 
  3  n_pe     F     0   100000  ! number of photoelectrons
!
 END TABLE
!
