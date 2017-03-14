!-----------------------------------------------------------------------
!       BANKname BANKtype                ! Comments
 TABLE  TGBI   B32  ! create write display delete ! Trigger bank
!
!   ATTributes:
!   -----------
!COL ATT-name 	    FMT  Min     Max     ! Comments
!     	      	    
  1 latch1          I      0    9999999  ! level1 trigger latch word (16 bits)
                       			 !   bits    descr
                       			 !   ----    -----
                       			 !    1-12   Level 1 input bits to trigger supervisor
                       			 !     13    not used
                       			 !     14    not used
                       			 !     15    Helicity clock
                       			 !     16    Helicity state
  2 helicity_scaler I      0    9999999  ! helicity interval count
  3 interrupt_time  I      0    9999999  ! interrupt time from microsec clock
  4 latch2          I      0    9999999  ! level2 trigger latch word (16 bits)
                                         !   bits    descr
                                         !   ----    -----
                                         !    1-8    level 2 sector bits
                                         !    9-12   not used
                                         !     13    level 2 fail
                                         !     14    level 2 pass
                                         !     15    level 2 fail
                                         !     16    level 2 pass
!
  5 level3          I      0    9999999  ! level3 trigger word (32 bits)
                                         !   bits    descr
                                         !   ----    -----
                                         !    1-6    level 3 sector bits
                                         !    7-24   not used
                                         !    25-32  version number
!
 END TABLE
!
