!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SHPB     ! create write display delete ! Scintillation Hodoscope reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min    Max   ! Comments
!
  1  id        I    101      502    ! channel number (quadrant*100 + channel)
  2  x         F  -1000.0   1000.0  ! lab coordinate X,
  3  y         F  -1000.0   1000.0  ! lab coordinate Y,
  4  z         F  -1000.0   1000.0  ! lab coordinate Z,
  5  nphe      F      0.0   1000.0  ! number of photoelectrons
  6  time      F -20000.0  20000.0  ! t
  7  status    I      0     1000    ! pixel hit status    
!
!
 END TABLE
!
