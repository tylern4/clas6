!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCGW   ! create display delete ! DC geometry info by wire - in sector coordinate system
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max          ! Comments
  1  x_mid     F  -1000.0  1000.0          ! x at midplane of wire(cm)
  2  y_mid     F  -1000.0  1000.0          ! y at midplane of wire(cm)
  3  z_mid     F  -1000.0  1000.0          ! z at midplane of wire(cm)
  4  x_dir     F  -1.0000  1.0000          ! x direction cosine along wire (cm)
  5  y_dir     F  -1.0000  1.0000          ! y direction cosine along wire (cm)
  6  z_dir     F  -1.0000  1.0000          ! z direction cosine along wire (cm)
  7  w_len     F   0.0     1000.0          ! wire length from midplane to amplifier (cm)
  8  w_len_hv  F   0.0     1000.0          ! wire length from midplane to HV (cm)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
