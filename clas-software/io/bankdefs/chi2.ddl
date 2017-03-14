!-----------------------------------------------------------------------
! Kinematic Fitter Information bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CHI2          ! create write display delete !Kinematic Fitter Info Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1       chi2   F  0.0    1000.0   ! overall chi2 for event  
2       cl     F  0.0    1.0      ! percentage (confidence level)
3       ndf    I  0      100      ! number of effective degrees of freedom
4       iter   I  0      1000     | number of iterations to converge
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
