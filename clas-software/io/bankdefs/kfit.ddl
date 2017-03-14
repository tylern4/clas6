!-----------------------------------------------------------------------
! Kinematic Fitter Result bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  KFIT          ! create write display delete !Kinematic Fitter Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     momenta_f  F  0  10.0         ! fitted momentum parameter (GeV)   
2     lambda_f   F  -1.0 1.0        ! fitted dip angle (radians) 
3     phi_f      F  -4.0 4.0        ! fitted phi angle (radians)
4     d0_f       F  -100.0 100.0    ! fitted d0 (cm)
5     z0_f       F  -100.0 100.0    ! fitted z0 (cm)
6     sigma11    F  0  10.0         !   error element of known
7     sigma12	 F  -10.0  10.0	    !   error element of known
8     sigma13    F  -10.0  10.0     !   error element of known
9     sigma14    F  -10.0  10.0     !   error element of known
10    sigma15    F  -10.0  10.0     !   error element of known
11    sigma22    F  0  10.0         !   error element of known   
12    sigma23    F  -10.0  10.0     !   error element of known
13    sigma24    F  -10.0  10.0     !   error element of known
14    sigma25    F  -10.0  10.0     !   error element of known
15    sigma33    F  0  10.0         !   error element of known 
16    sigma34    F  -10.0  10.0     !   error element of known 
17    sigma35    F  -10.0  10.0     !   error element of known 
18    sigma44    F  0  10.0         !   error element of known 
19    sigma45    F  -10.0  10.0     !   error element of known 
20    sigma55    F  0  10.0         !   error element of known 
21    cov11      F  -10.0  10.0     !   error element bewteen known and unknown
22    cov12	 F  -10.0  10.0     !   error element bewteen known and unknown
23    cov13      F  -10.0  10.0     !   error element bewteen known and unknown
24    cov14      F  -10.0  10.0     !   error element bewteen known and unknown
25    cov15      F  -10.0  10.0     !   error element bewteen known and unknown
26    cov21      F  -10.0  10.0     !   error element bewteen known and unknown
27    cov22      F  -10.0  10.0     !   error element bewteen known and unknown
28    cov23      F  -10.0  10.0     !   error element bewteen known and unknown
29    cov24      F  -10.0  10.0     !   error element bewteen known and unknown
30    cov25      F  -10.0  10.0     !   error element bewteen known and unknown
31    cov31      F  -10.0  10.0     !   error element bewteen known and unknown
32    cov32      F  -10.0  10.0     !   error element bewteen known and unknown
33    cov33      F  -10.0  10.0     !   error element bewteen known and unknown
34    cov34      F  -10.0  10.0     !   error element bewteen known and unknown
35    cov35      F  -10.0  10.0     !   error element bewteen known and unknown
36    cov41      F  -10.0  10.0     !   error element bewteen known and unknown
37    cov42      F  -10.0  10.0     !   error element bewteen known and unknown
38    cov43      F  -10.0  10.0     !   error element bewteen known and unknown
39    cov44      F  -10.0  10.0     !   error element bewteen known and unknown
40    cov45      F  -10.0  10.0     !   error element bewteen known and unknown
41    cov51      F  -10.0  10.0     !   error element bewteen known and unknown
42    cov52      F  -10.0  10.0     !   error element bewteen known and unknown
43    cov53      F  -10.0  10.0     !   error element bewteen known and unknown
44    cov54      F  -10.0  10.0     !   error element bewteen known and unknown
45    cov55      F  -10.0  10.0     !   error element bewteen known and unknown
46    chi2piece  F  -10.0  10.0     ! track contribution to chi2
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
