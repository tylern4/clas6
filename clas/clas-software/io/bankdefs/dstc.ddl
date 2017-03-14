!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSTC       B16  ! create write display delete   ! Down Stream Total Absorption Shower Counter event bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  TACID    I     0    1     ! ID compelled by DAQ (always 1)
  2  TDCTAC   I     0    4096  ! tdc information (on sum)
  3  ADCLT    I     0   65536  ! adc information (left top)
  4  ADCRT    I     0   65536  ! adc information (right top)
  5  ADCLB    I     0   65536  ! adc information (left bottom)
  6  ADCRB    I     0   65536  ! adc information (right bottom)
  7  ADCSUM1  I     0   65536  ! adc information (sum scale1)
  8  ADCSUM2  I     0   65536  ! adc information (sum scale2)
  9  ADCSUM3  I     0   65536  ! adc information (sum scale3)
!
 END TABLE
!
