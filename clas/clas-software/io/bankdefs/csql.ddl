!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   CSQL   B32   ! create write display delete ! Data bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 EVID     I    1    100000 ! Event ID (number of triggers)
   2 NPROC    I    1    100000 ! Number of processed triggers
   3 CPU      F    0.    99999.! CPU used (sec) 
   4 TG       F    0.      999.! Clock Gated
   5 IBEAM    F    0.      999.! Beam current 
   6 NpipS1   I    0    100000 ! Number of pi+ in sect 1
   7 NpipS2   I    0    100000 ! Number of pi+ in sect 2
   8 NpipS3   I    0    100000 ! Number of pi+ in sect 3 
   9 NpipS4   I    0    100000 ! Number of pi+ in sect 4 
  10 NpipS5   I    0    100000 ! Number of pi+ in sect 5  
  11 NpipS6   I    0    100000 ! Number of pi+ in sect 6
  12 NprotS1  I    0    100000 ! Number of prot in sect 1
  13 NprotS2  I    0    100000 ! Number of prot in sect 2
  14 NprotS3  I    0    100000 ! Number of prot in sect 3 
  15 NprotS4  I    0    100000 ! Number of prot in sect 4 
  16 NprotS5  I    0    100000 ! Number of prot in sect 5  
  17 NprotS6  I    0    100000 ! Number of prot in sect 6
  18 NpimS1   I    0    100000 ! Number of pi- in sect 1
  19 NpimS2   I    0    100000 ! Number of pi- in sect 2
  20 NpimS3   I    0    100000 ! Number of pi- in sect 3 
  21 NpimS4   I    0    100000 ! Number of pi- in sect 4 
  22 NpimS5   I    0    100000 ! Number of pi- in sect 5  
  23 NpimS6   I    0    100000 ! Number of pi- in sect 6
  24 NhbS1    I    0    100000 ! Number of hit-based tracks in sect 1
  25 NhbS2    I    0    100000 ! Number of hit-based tracks in sect 2
  26 NhbS3    I    0    100000 ! Number of hit-based tracks in sect 3 
  27 NhbS4    I    0    100000 ! Number of hit-based tracks in sect 4 
  28 NhbS5    I    0    100000 ! Number of hit-based tracks in sect 5  
  29 NhbS6    I    0    100000 ! Number of hit-based tracks in sect 6
  30 NtbS1    I    0    100000 ! Number of time-based tracks in sect 1
  31 NtbS2    I    0    100000 ! Number of time-based tracks in sect 2
  32 NtbS3    I    0    100000 ! Number of time-based tracks in sect 3 
  33 NtbS4    I    0    100000 ! Number of time-based tracks in sect 4 
  34 NtbS5    I    0    100000 ! Number of time-based tracks in sect 5  
  35 NtbS6    I    0    100000 ! Number of time-based tracks in sect 6
  36 NphotS1  I    0    100000 ! Number of photons in sect 1
  37 NphotS2  I    0    100000 ! Number of photons in sect 2
  38 NphotS3  I    0    100000 ! Number of photons in sect 3 
  39 NphotS4  I    0    100000 ! Number of photons in sect 4 
  40 NphotS5  I    0    100000 ! Number of photons in sect 5  
  41 NphotS6  I    0    100000 ! Number of photons in sect 6
  42 Nhb      I    0   1000000 ! Number of HB 
  43 Ntb      I    0   1000000 ! Number of TB
  44 Nprot    I    0   1000000 ! Number of protons
  45 Npip     I    0   1000000 ! number of pip
  46 Npim     I    0   1000000 ! number of pim
  47 Nphot    I    0   1000000 ! number of photons	
  48 Nkp      I    0   1000000 ! Number of K+ 
  49 Nkm      I    0   1000000 ! Number of K- 
  50 Ngamma   I    0   1000000 ! Number of photons in the tagger from scaler
!
 END TABLE