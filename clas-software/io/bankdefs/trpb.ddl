!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TRPB  ! create write display delete ! Tracking Results for Layer 1
!        (additional tracking bank for EG1 that should be added to *BER)
!  record_no=0
!       hits_hbt, hits_tbt:  #hits per superlayer used in fit (decimal coded)
!                          nhits_per_superlayer *10^(superlayer-1)
!                             plus  10^6 for all 6 superlayers in fit
!       hits_tbt:  DC hits used in TBT fit  (i.e. those which passed 
!                                                  left-right-ambiguity fits)
!       
!  	coordinates/directions for first DC layer in CLAS coordinate system
!       (first DC layer = layer 1, except for special fit flags: bits5-7)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  hits_tbt   I      0  1666666   ! hits used in TBT fit
  2  x          F   -300.    300.   ! x
  3  y          F   -300.    300.   ! y   position in first DC layer 
  4  z          F   -300.    300.   ! z
  5  cx         F     -1.      1.   ! cx
  6  cy         F     -1.      1.   ! cy  dir.cosine at first DC layer
  7  cz         F     -1.      1.   ! cz
  8  tlen       F      0.    300.   ! track length to this layer (starting
!                                      from 'vertex' in HBTR / TBTR
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
