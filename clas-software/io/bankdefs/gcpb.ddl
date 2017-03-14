!********************************************************************
!BANKname      BANKtype                    ! Comments
 TABLE  GCPB ! create write display delete ! GEM Compound particle bank for BONUS
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  pid        I   -5000   5000    ! particle id (PDG)
  2  x          F   -100.   100.    ! vector3_t vert; Vertex position {x,y,z} 
  3  y          F   -100.   100.    ! y
  4  z          F   -500.   500.    ! z
  5  dEdx       F   0.      200.    ! Energy / 
  6  px         F   -16.    16.     ! momentum {px,py,pz}
  7  py         F   -16.    16.     ! py
  8  pz         F   -16.    16.     ! pz
  9  p_tot      F   -16.    16.     ! total momentum  
 10  x2         F    0      1000.   ! chi square if the fitted track
 11  theta      F   -1000.  1000.   ! theta of the track
 12  charge     F   -1000.  1000.   ! total charge
 13  dca        F   -1000.  1000.   ! distance of closest approach
 14  index      I    1      100     ! link to (uncertainties?)bank yet to be crated 
 15  phi        F   -1000.  1000.   ! phi of the track
 16  vtl        F   -1000.  1000.   ! visual track length
 17  sdist      F   -1000.  1000.   ! distance 
 18  edist      F   -1000.  1000.   ! distance
 19  npts       F   -1000.  1000.   ! number of data points of track
 20  r_0        F   -1000.  1000.   ! radius of curviture
!
END TABLE
