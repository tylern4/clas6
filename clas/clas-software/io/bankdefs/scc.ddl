!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCC                 ! create write display delete ! GSIM & SC Scintillator calibration info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
   1   id         I       1       0x00FF     ! paddle id# 
   2   date       I       0       0xFFFF     ! UNIX univiersal time of calibraton (32 bits) 
   3  version     I       0       0xFFFF     ! sequential version# of calibration 
   4  status      I       0       0xFFFF     ! 4 byte status word (see details) 
   5    TD0L      F      -999999.9 999999.9      ! (Left) gives 0 time at center for tube (ns) 
   6    TD0Lu     F      -999999.9 999999.9      ! (Left) uncertainty (ns) 
   7    TD0R      F      -999999.9 999999.9      ! (Right) gives 0 time at center for tube (ns) 
   8    TD0Ru     F      -999999.9 999999.9      ! (Right) uncertainty (ns) 
   9    TD1L      F      -999999.9 999999.9      ! (Left) TDC**1 coefficient (ns/ch) 
 10    TD1Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns/ch) 
 11    TD1R       F      -999999.9 999999.9      ! (Right) TDC**1 coefficient (ns/ch) 
 12    TD1Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns/ch) 
 13    TD2L       F      -999999.9 999999.9      ! (Left) TDC**2 coefficient 
 14    TD2Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns/ch) 
 15    TD2R       F      -999999.9 999999.9      ! (Right)TDC**2 coefficient 
 16    TD2Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns/ch) 
 17    TW0L       F      -999999.9 999999.9      ! (Left) time walk constant (ns) 
 18    TW0Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns) 
 19    TW0R       F      -999999.9 999999.9      ! (Right) time walk constant (ns) 
 20    TW0Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns) 
 21    TW1L       F      -999999.9 999999.9      ! (Left) time walk 1st factor 
 22    TW1Lu      F      -999999.9 999999.9      ! (Left) uncertainty 
 23    TW1R       F      -999999.9 999999.9      ! (Right) time walk 1st factor 
 24    TW1Ru      F      -999999.9 999999.9      ! (Right) uncertainty 
 25    TW2L       F      -999999.9 999999.9      ! (Left) time walk 2nd factor 
 26    TW2Lu      F      -999999.9 999999.9      ! (Left) uncertainty 
 27    TW2R       F      -999999.9 999999.9      ! (Right) time walk 2nd factor 
 28    TW2Ru      F      -999999.9 999999.9      ! (Right) uncertainty 
 29    ADPL       F      -999999.9 999999.9      ! (Left) ADC pedestal (ch) 
 30    ADPLu      F      -999999.9 999999.9      ! (Left) uncertainty 
 31    ADPR       F      -999999.9 999999.9      ! (Right) ADC pedestal (ch) 
 32    ADPRu      F      -999999.9 999999.9      ! (Right) uncertainty 
 33    M0L        F      -999999.9 999999.9      ! (Left) nmip adc channel 
 34    M0Lu       F      -999999.9 999999.9      ! (Left) uncertainty (ch) 
 35    M0R        F      -999999.9 999999.9      ! (Right)nmip adc channel 
 36    M0Ru       F      -999999.9 999999.9      ! (Right) uncertainty(ch) 
 37    VEFL       F      -999999.9 999999.9      ! (Left) effective velocity of light (cm/ns) 
 38    VEFLu      F      -999999.9 999999.9      ! (Left) uncertainty (cm/ns) 
 39    VEFR       F      -999999.9 999999.9      ! (Right) effective velocity of light (cm/ns) 
 40    VEFRu      F      -999999.9 999999.9      ! (Right) uncertainty (cm/ns) 
 41    ATNL       F      -999999.9 999999.9      ! (Left) attenuation length (cm) 
 42    ATNLu      F      -999999.9 999999.9      ! (Left) uncertainty (cm) 
 43    ATNR       F      -999999.9 999999.9      ! (Right) attenuation length (cm) 
 44    ATNRu      F      -999999.9 999999.9      ! (Right) uncertainty (cm) 
 45    TSIG0L     F      -999999.9 999999.9      ! (Left) 1st parameter of measured resolution (ns) 
 46    TSIG0R     F      -999999.9 999999.9      ! (Right) 1st parameter of measured resolution (ns) 
 47    TSIG1L     F      -999999.9 999999.9      ! (Left) 2nd parameter of measured resolution 
 48    TSIG1R     F      -999999.9 999999.9      ! (Right) 2nd parameter of measured resolution 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
