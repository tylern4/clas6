!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBTR  ! create write display delete ! Hit Based Tracking Result bank
! record_no= 0
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
! (changed F.Klein Nov.97)
! col.9: trackno_in_Sector = track# in the Sector based Tracking Banks (HBLA,HDPL..)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  x          F    -50.     50.   ! x
  2  y          F    -50.     50.   ! y 'Vertex' position {x,y,z}
  3  z          F    -50.     50.   ! z
  4  px         F    -10.     10.   ! Px
  5  py         F    -10.     10.   ! Py  momentum at 'vertex' {Px,Py,Pz}
  6  pz         F    -10.     10.   ! Pz
  7  q          F     -1.      1.   ! charge   (straight tracks: set q=0.)
  8  chi2       F      0.     50.   ! Chisquare for this Track
  9  itr_sec    I    101     620    ! Trackno_in_Sector + Sector*100
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE

