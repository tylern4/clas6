!-----------------------------------------------------------------------
!       BANKname BANKtype                ! Comments
 TABLE  RC26   B32  ! create write display delete ! Trigger bank
!
!   ATTributes:
!   -----------
!COL ATT-name 	    FMT  Min     Max     ! Comments
!     	      	    
  1 trigger_pattern I      0    9999999  ! level1 trigger pattern from trigger supervisor
  2 gated_scaler    I      0    9999999  ! gated scaler
  3 ungated_scaler  I      0    9999999  ! ungated scaler
!
 END TABLE
!
