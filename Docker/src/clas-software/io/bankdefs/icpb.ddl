!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ICPB    ! create write display delete ! IC hits corrected as EM shower
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  Etot     F     0.   20.     ! Reconstructed total energy 
  2  Ecen     F     0.   20.     ! Energy in the central crystal
  3  Time     F    -5.  150.     ! Time relative to the evnt start time
  4  T_next     F -1000.0 1000.0  ! Time to next closest hit
  5  X        F    -1.    1.     ! x coordinate of hit 
  6  Y        F    -1.    1.     ! y coordinate of hit
  7  Z        F    -1.    1.     ! z coordinate of hit (front face of IC)
  8  M2_hit   F   0.0  9999.0    ! second moment of _hit pattern
  9  M3_hit   F -9999.0  9999.   ! third moment of  _hit pattern 
  10  Status   I     0   0xFFFF   ! Ncrystals+10000*Hit_ID in ICHB 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
