!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HCAL	! create   delete ! Header CALibration bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  VERSION    I   0         2 ! Version number
  2  RUN_LOW    I   1      1000 ! LOW RUN number		 
  3  RUN_HIGH   I   1      1000 ! HIGH RUN number
  4  CATIME     I 100 100000000 ! CAlibration TIME (unix time = 0 1/1/1970)		
  5  ROCCA      I   0        20 ! 32 bit ReadOut Controller CAlibration status
                                ! 1 - Drift Chamber Calibration	
                                ! 2 - 
!
 END TABLE
! 
!-------------------------------------------------------------------------------
