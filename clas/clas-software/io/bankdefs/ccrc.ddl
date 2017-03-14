!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCRC           ! create write display delete ! Cerenkov reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  nrsect      I  1      6      ! Sector #
  2  nrsegm      I  0      180    ! 10 * Mean segment #
  3  nrsegm_p    I  0      180    ! 10 * Max segment # in the cluster
  4  nrsegm_m    I  0      180    ! 10 * Min segment # in the cluster
  5  nrphe       I  0      100    ! Number of photoelectrons obtained
  6  nrtime      I  0      1000   ! TOF in channels (50psec/channel)
  7  nrthet      I  0      180    ! Estimated angle Theta
  8  nrdthet     I  0      180    ! Estimated error of angle Theta
  9  nrphy       I  -1     1      ! Phy index 
!            ( -1 if Phy < 0 ; 1 if Phy > 0 ; 0 if Phy ~ 0)
 10   nriec      I  -500   500    ! Estimated i-coordinate in EC 
 11   nrdiec     I  0      500    ! Estimated error of i-coord. in EC
 12   nrstat     I  -10    10     ! Status  word (yet unclear) 
!
!  All distances in cm , all angles in degrees
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
