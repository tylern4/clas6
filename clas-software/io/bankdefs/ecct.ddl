!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCT                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  tOFF     F       0 99999.0  ! TDC offset (channels)
  3  tOFFu    F       0 99999.0  ! tOFF Error
  4  tGAIN    F       0  1000.0  ! TDC conversion gain (nS/channel)
  5  tGAINu   F       0  1000.0  ! tGAIN Error
  6  tW0      F -9999.0  9999.0  ! Time walk constant (channels)
  7  tW0u     F -9999.0  9999.0  ! tWOu Error
  8  tW1      F -9999.0  9999.0  ! Time walk correction parameter 
  9  tW1u     F -9999.0  9999.0  ! tW1u Error
 10  tVEF     F       0    10.0  ! Effective velocity of light (cm/ns)
 11  tVEFu    F       0    10.0  ! tVEFu Error
 12  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
