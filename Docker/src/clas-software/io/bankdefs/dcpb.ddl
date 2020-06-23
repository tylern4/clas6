!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCPB    ! create write display delete ! DC tracks involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScTr     I     0  6010    ! 100*sector+track_ID in *BTR  
  2  x_SC     F  -999.  999.   ! x coordinate of track intersection with SC plane 
  3  y_SC     F  -999.  999.   ! y coordinate of track intersection with SC plane
  4  z_SC     F  -999.  999.   ! z coordinate of track intersection with SC plane
  5  CX_SC    F    -1.    1.   ! X dir cosine at (x_SC,y_SC,z_SC)
  6  CY_SC    F    -1.    1.   ! y dir cosine at (x_SC,y_SC,z_SC)
  7  CZ_SC    F    -1.    1.   ! z dir cosine at (x_SC,y_SC,z_SC)
  8  X_v      F  -999.  999.   ! vertex X after fiting to the beam position
  9  Y_v      F  -999.  999.   ! vertex Y after fiting to the beam position
 10  Z_v      F  -999.  999.   ! vertex Z after fiting to the beam position
 11  R_v      F     0.  180.   ! distance from production vertex to the bemam. 
 12  Chi2     F     0.  100.   ! Chisquare of track fitting
 13  Status   I     0   0xFFFF ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
