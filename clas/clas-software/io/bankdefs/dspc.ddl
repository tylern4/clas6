!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSPC       B16  ! create write display delete ! Down Stream Pair Counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  PCID     I     0       1  ! Id compelled by DAQ (always 1)
  2  TDCPC    I     0    4096  ! tdc information ( scintillator)
  3  ADCMN    I     0   65536  ! adc information (main)
  4  ADCLT    I     0   65536  ! adc information (left top)
  5  ADCRB    I     0   65536  ! adc information (right bottom)
  6  ADCLB    I     0   65536  ! adc information (left bottom)
  7  ADCRT    I     0   65536  ! adc information (right top)
  8  ADCVE    I     0   65536  ! adc information (veto)
!
 END TABLE
!
