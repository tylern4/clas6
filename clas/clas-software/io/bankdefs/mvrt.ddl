!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  MVRT  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  v_id     	I   -1000    1000   ! info about track ids
  2  ntrk       F   -100.    100.   ! number of tracks used to make vertex
  3  x          F  -1000.   1000.   ! x vector3_t vert{x,y,z}
  4  y          F  -1000.   1000.   ! y 
  5  z          F  -1000.   1000.   ! z
  6  chi2       F  -1000.   1000.   ! chi2
  7  cxx        F  -1000.   1000.   ! Covariance matrix array element
  8  cxy        F  -1000.   1000.   ! Covariance matrix array element
  9  cxz        F  -1000.   1000.   ! Covariance matrix array element
 10  cyy        F  -1000.   1000.   ! Covariance matrix array element
 11  cyz        F  -1000.   1000.   ! Covariance matrix array element
 12  czz        F  -1000.   1000.   ! Covariance matrix array element
 13  stat       I  -1000.   1000.   ! status integer, not used yet
!
! note v_id is based upon the track id used to make the
! vertex.  v_id = (summed over all tracks used) 2^(tber id of track(1-10)) 
!                   + 1 if beamline info used
 END TABLE


