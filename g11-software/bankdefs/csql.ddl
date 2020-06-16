!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   CSQL   B32   ! create write display delete ! Data bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 EVID     I    1    100000 ! Event ID (number of triggers)
   2 NPROC    I    1    100000 ! Number of processed triggers
   3 CPU      F    0.    99999.! CPU used (sec) 
   4 FC       F    0.      999.! Faraday Cup (K)
   5 FCG      F    0.      999.! Faraday Cup Gated (K)
   6 TG       F    0.      999.! Clock Gated
   7 IBEAM    F    0.      999.! Beam current 
   8 NpipS1   I    0    100000 !  Number of pi+ in sect 1
   9 NpipS2   I    0    100000 !  Number of pi+ in sect 2
  10 NpipS3   I    0    100000 !  Number of pi+ in sect 3 
  11 NpipS4   I    0    100000 !  Number of pi+ in sect 4 
  12 NpipS5   I    0    100000 !  Number of pi+ in sect 5  
  13 NpipS6   I    0    100000 !  Number of pi+ in sect 6
  14 Nhb      I    0   1000000 ! Number of HB 
  15 Ntb      I    0   1000000 ! Number of TB
  16 Nprot    I    0   1000000 ! Number of protons
  17 Npip     I    0   1000000 ! number of pip
  18 Ndeut    I    0   1000000 ! number of deutrons
  19 Nphot    I    0   1000000 ! number of photons	
  20 Nepiphp  I    0   1000000 ! Number of pi+ at pos. Helic.
  21 Npiphn   I    0   1000000 ! Number of pi+ at neg. helic.
  22 Ngamma   I    0   1000000 ! Number of photons in the tagger from scaler
  23 Ng_norm  I    0   1000000 ! Number of photons from gflux
!
 END TABLE
