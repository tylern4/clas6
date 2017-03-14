!-----------------------------------------------------------------------
! Time-of-flight Cluster (final result) bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCRC          ! create write display delete !Sc Scintillator reconstruction hit bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     id         I  0  48           ! cluster id   
2     energy     F  0  100000.0     ! cluster Energy (MeV) 
3     denergy    F  0  100000.0     ! error in cluster energy (ns)
4     time       F  0  100000.0     ! cluster (energy-weighted) time(ns) 
5     dtime	 F  0  100000.0	    ! error in cluster time (ns)
6     x       F -999999.9  999999.9  ! x position in sector coordinate system 
7     y       F -999999.9  999999.9  ! y position in sector coordinate system 
8     z       F -999999.9  999999.9  ! z position in sector coordinate system 
9     dx      F -999999.9  999999.9  ! x error in sector coordinate system 
10    dy      F -999999.9  999999.9  ! y error in sector coordinate system 
11    dz      F -999999.9  999999.9  ! z error in sector coordinate system 
12    status  I  -999999    999999   ! status word defined in sc.h
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
