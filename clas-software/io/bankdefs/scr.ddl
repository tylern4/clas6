!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCR          ! create write display delete !Sc Scintillator reconstruction hit bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     id         I  0  48           ! paddle id  
2     energy     F  0  100000.0     ! Energy (MeV) 
3     time       F  0  100000.0     ! time(ns)
4     x       F -999999.9  999999.9  ! x position in sector coodinate system 
5     y       F -999999.9  999999.9  ! y position in sector coodinate system 
6     z       F -999999.9  999999.9  ! z position in sector coodinate system 
7     dx      F -999999.9  999999.9  ! x error in sector coodinate system 
8     dy      F -999999.9  999999.9  ! y error in sector coodinate system 
9     dz      F -999999.9  999999.9  ! z error in sector coodinate system 
10    status  I  -999999    999999   ! status word defined in sc.h
11    denergy    F  0  100000.0     ! uncertainty in Energy (MeV) 
12    dtime      F  0  100000.0     ! uncertainty in time (ns) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
