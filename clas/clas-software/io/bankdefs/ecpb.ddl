!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScHt     I     0  6010      ! 100*sector+Whole_Hit_ID in ECHB 
  2  Etot     F     0.   20.     ! Reconstructed total energy
  3  Ein      F     0.   20.     ! Inner energy
  4  Eout     F     0.   20.     ! Outer energy 
  5  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  6  Path     F     0.  600.     ! Path lenght from target
  7  X        F    -1.    1.     ! x coordinate of hit 
  8  Y        F    -1.    1.     ! y coordinate of hit
  9  Z        F    -1.    1.     ! z coordinate of hit
 10  M2_hit   F   0.0  9999.0    ! second moment of _hit pattern
 11  M3_hit   F -9999.0  9999.   ! third moment of  _hit pattern 
 12  M4_hit   F     0.  9999.    ! forth moment of  _hit pattern 
 13  InnStr   I     0. 40000.    ! 10000*UI+100*VI+WI 
 14  OutStr   I     0. 40000.    ! 10000*UO+100*VO+WO 
 15  Chi2EC   F     0.   100.    ! Quality measure of geometrical matching
 16  Status   I     0   0xFFFF   ! Status word (not implemented yet)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
