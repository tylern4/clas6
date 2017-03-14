!********************************************************************
!BANKname      BANKtype                    ! Comments
 TABLE  GPID ! create write display delete ! ST-SC PID bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  pid        I   0       100     ! particle id (GEANT)
  2  x          F   -100.   100.    ! vector3_t vert; Vertex position {x,y,z} 
  3  y          F   -100.   100.    ! y
  4  z          F   -500.   500.    ! z
  5  E          F   0.      16.     ! vector4_t p; Energy
  6  px         F   -16.    16.     ! momentum {x,y,z}
  7  py         F   -16.    16.     !  py
  8  pz         F   -16.    16.     ! pz
  9  q          I   -16     16      ! charge
 10  trkid      I   -16.    16.     ! index to TBID bank, counting from 1
 11  sec        I   1       6       ! Sector track is in
 12  paddle     I   0       50      ! Paddle hit
 13  dedx       F   -10000. 10000   ! Energy deposited in TOF
 14  beta       F   -1.     1.      ! beta pmag/E
 15  sc_stat    I    0      10      ! status of hit matching to SC: see sc.h
 16  sc_time    F   -10000. 10000.  ! SC calibrated time for this track (ns)
 17  sc_len     F   -10000. 10000.  ! track length [cm] from origin to SC
 18  st_stat    I   0       10      ! ST status
 19  st_time    F   -10000. 10000.  ! ST calibrated time for this track (ns) 
 20  st_len     F   -10000. 10000.  ! track length [cm] from origin to ST
 21  mass       F   0.      1000.   ! mass from time-of-flight
 22  mass_ref   I   -10     10      ! 0: mass calc from SC & TAG, 1: SC & ST; -1 neutral or no SC;2:from PART
 23  betam      F   -1.     1.      ! beta from time_of-flight
 24  epho       F   0.      12.     ! closest photon energy (GeV)
 25  tpho       F   -1000.  1000.   ! Time of the photon after RF correction
 26  tagrid     I   0       4096    ! index to TAGR bank, counting from 1
 27  ngrf       I   0       1000    ! number of photons in the same RF bucket
 28  ppid       I   0       100     ! pid as seen in PART bank
!
END TABLE
