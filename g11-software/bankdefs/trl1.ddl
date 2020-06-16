!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TRL1  ! create write display delete ! Tracking Results for Layer 1
!        (additional tracking bank for EG1 that should be added to *BER)
!  record_no=0
!    (additional record_no=1 only after time based tracking: 
!              for all hit based tracks that didn't make it through 
!              time based tracking (after skipping 'pseudo' tracks))
!
!  Track# = row#  (cf. HBTR / TBTR banks)
!       fit_flags:  flags which were used in the fitting  (bit pattern)
!               bit0: transverse vertex constraint (width via dpar_SigmaBeam)
!               bit1: longitudinal vx. constraint (length via dpar_TargetLen)
!               bit2: propagation to variable vertex (otherwise: beamline)
!               bit8:  fit in DC layer 1 to 12 
!               bit9:  fit in DC layer 13 to 24 
!               bit10: fit in DC layer 25 to 36 
!               bit11: fit in DC axial superlayers only
!
!       hits_hbt, hits_tbt:  #hits per superlayer used in fit (decimal coded)
!                          nhits_per_superlayer *10^(superlayer-1)
!                             plus  10^6 for all 6 superlayers in fit
!       hits_hbt:  DC hits used in HBT fit  (usually all existing DC hits
!                                                           on the track)
!       hits_tbt:  DC hits used in TBT fit  (i.e. those which passed 
!                                                  left-right-ambiguity fits)
!       
!  	coordinates/directions for first DC layer in CLAS coordinate system
!       (first DC layer = layer 1, except for special fit flags: bits5-7)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  fit_flags  I      0    65536   ! flags used in track fitting
  2  hits_hbt   I      0  1666666   ! hits used in HBT fit
  3  hits_tbt   I      0  1666666   ! hits used in TBT fit
  4  x          F   -300.    300.   ! x
  5  y          F   -300.    300.   ! y   position in first DC layer 
  6  z          F   -300.    300.   ! z
  7  cx         F     -1.      1.   ! cx
  8  cy         F     -1.      1.   ! cy  dir.cosine at first DC layer
  9  cz         F     -1.      1.   ! cz
 10  tlen       F      0.    300.   ! track length to this layer (starting
!                                      from 'vertex' in HBTR / TBTR
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
