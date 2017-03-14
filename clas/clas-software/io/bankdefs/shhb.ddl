!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SHHB     ! create write display delete ! Scintillation Hodoscope reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min    Max   ! Comments
!
  1  hid        I    101      502    ! channel number (quadrant*100 + channel)
  2  hx         F  -1000.0   1000.0  ! lab coordinate X,
  3  hy         F  -1000.0   1000.0  ! lab coordinate Y,
  4  hz         F  -1000.0   1000.0  ! lab coordinate Z,
  5  hnphe      F      0.0   1000.0  ! number of photoelectrons
  6  htime      F -20000.0  20000.0  ! t
  7  hstatus    I      0     1000    ! channel hit status    
!
!
 END TABLE
!
