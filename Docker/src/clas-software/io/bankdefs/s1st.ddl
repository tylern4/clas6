!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  S1ST    ! create write display delete ! Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 latch1_bit1_count       I     0    9999999  ! Count trigger bit 1  latched events
  2 latch1_bit2_count       I     0    9999999  ! Count trigger bit 2  latched events
  3 latch1_bit3_count       I     0    9999999  ! Count trigger bit 3  latched events
  4 latch1_bit4_count       I     0    9999999  ! Count trigger bit 4  latched events
  5 latch1_bit5_count       I     0    9999999  ! Count trigger bit 5  latched events
  6 latch1_bit6_count       I     0    9999999  ! Count trigger bit 6  latched events
  7 latch1_bit7_count       I     0    9999999  ! Count trigger bit 7  latched events
  8 latch1_bit8_count       I     0    9999999  ! Count trigger bit 8  latched events
  9 latch1_bit9_count	    I     0    9999999  ! Count trigger bit 9  latched events
 10 latch1_bit10_count	    I     0    9999999  ! Count trigger bit 10 latched events
 11 latch1_bit11_count	    I     0    9999999  ! Count trigger bit 11 latched events
 12 latch1_bit12_count	    I     0    9999999  ! Count trigger bit 12 latched events
 13 event_count 	    I     0    9999999  ! Latched event count this run
 14 latch1_zero_count	    I     0    9999999  ! Latch1 zero count (illegal)
 15 latch1_empty_count	    I     0    9999999  ! Latch1 empty count (illegal)
 16 latch1_not_empty_count  I     0    9999999  ! Latch1 not empty on sync event (illegal)
 17 latch1_ok_count         I     0    9999999  ! Latch1 ok
 18 level2_sector1          I     0    9999999  ! Level2 sector1 count
 19 level2_sector2          I     0    9999999  ! Level2 sector2 count
 20 level2_sector3          I     0    9999999  ! Level2 sector3 count
 21 level2_sector4          I     0    9999999  ! Level2 sector4 count
 22 level2_sector5          I     0    9999999  ! Level2 sector5 count
 23 level2_sector6          I     0    9999999  ! Level2 sector6 count
 24 level2_pass             I     0    9999999  ! Level2 pass count
 25 level2_fail             I     0    9999999  ! Level2 fail count
 26 latch2_zero_count	    I     0    9999999  ! Latch2 zero count (illegal)
 27 latch2_empty_count	    I     0    9999999  ! Latch2 empty count (illegal)
 28 latch2_not_empty_count  I     0    9999999  ! Latch2 not empty on sync event (illegal)
 29 latch2_ok_count         I     0    9999999  ! Latch2 ok
 30 roc_13_count            I     0    9999999  ! Roc code 13 count (zero latch)
 31 roc_15_count            I     0    9999999  ! Roc code 15 count (illegal)
 32 l1l2_zero_count         I     0    9999999  ! (latch1==0)&&(latch2==0)
 33 l1zero_13_count         I     0    9999999  ! (latch1==0)&&(roc_code==13)
 34 l2zero_13_count         I     0    9999999  ! (latch2==0)&&(roc_code==13)
 35 l1l2zero_13_count       I     0    9999999  ! (latch1==0)&&(latch2==0)&&(roc_code==13)
!
 END TABLE
!
