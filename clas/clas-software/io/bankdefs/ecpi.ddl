!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPI        ! create write display delete ! Calorimeter pixels for DISPLAY only
!                                       
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1       6  ! sector number
  2  Layer     I     1       6  ! layer number, note 1-inner, 2-outer, 3-whole
  3  HITID     I     1      10  ! number of hits (first 16 bits) and hit ID (last 16 bits)
  4  iloc      F  -999.  9999.  ! position of the hit in the local coordinate system
  5  jloc      F  -999.  9999.  ! position of the hit in the local coordinate system
  6  diloc     F  -999.  9999.  ! i width of the hit
  7  djloc     F  -999.  9999.  ! j width of the hit 
  8  R         F     0.  9999.  ! radius of the shower
  9  E         F     0.  9999.  ! energy
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
