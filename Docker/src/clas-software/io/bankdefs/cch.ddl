!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCH          ! create  display delete ! GSIM Cerenkov hit info
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  tknum       i  0      0xFFFF     ! track number, 1000*istak+itra
  2  id          i  -4000  4000       ! track PDG id
  3  nhits       i  0      400        ! number of CC hits per track
  4  sector      i  1      6          ! sector number of track
  5  segment     i  1      18         ! cc segment number of track
  6  pmom        i  0.     20.        ! track momentum 
  7  xin         F  -1000. 1000.      ! x pos track entry into cerenkov
  8  yin         F  -1000. 1000.      ! y pos
  9  zin         F  -1000. 1000.      ! z pos
 10  xout        F  -1000. 1000.      ! x pos track exit from cerenkov
 11  yout        F  -1000. 1000.      ! y pos
 12  zout        F  -1000. 1000.      ! z pos
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
