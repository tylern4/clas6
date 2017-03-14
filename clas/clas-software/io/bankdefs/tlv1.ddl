!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TLV1     ! create write display delete ! Level1 trigger bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    3000  ! id = Detector * 256 + channel
                               ! Detector = 1 for the CC   (16 input channels)
                               ! Detector = 2 for the TOF  (32 input channels doubles and triples)
                               ! Detector = 3 for the EC   (16 input channels)
                               ! Detector = 4 for the LAC  (16 input channels)
							   ! Detector = 5 for TS inputs
							   ! channel 1 Async 1
                               ! channel 2 Async 2
                               ! channel 3 MOR 1
                               ! channel 4 Start Counter 1
                               ! channels 5 -- 16 not used
                               ! channel 17 Prescale 1
                               ! channel 18 Prescale 2
                               ! channel 19 Prescale 3
                               ! channel 20 Prescale 4
                               ! channel 21 Prescale 5
                               ! channel 22 Prescale 6
                               ! channel 23 Prescale 7
                               ! channel 24 Prescale 8
                               ! channel 25 Prescale 9
                               ! channel 26 Prescale 10
                               ! channel 27 Prescale 11
                               ! channel 28 Prescale 12
                               ! channel 29 Start Counter 2
                               ! channel 30 MOR 2
                               ! channel 31 Helicity bit
                               ! channel 32 Helicity bit
   2  time     F     0   65536 ! time(ns) 
!
 END TABLE
!




