!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TGPB    ! create write display delete ! Tagger hits matched start counter
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  pointer    I     0    100     ! 1000*i+tagstat where "i" is the pointer to the TAGR bank. Negative for trigger photon  
  2  Time       F -1000. 1000.     ! starttime_TAG at interaction point(ns)
  3  Energy     F     0.  100.     ! photon energy(GeV)
  4  dt     	F -1000. 1000.     ! starttime_ST - starttime_TAG (ns)
!				     if no starttime_ST, dt = -starttime_TAG
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
