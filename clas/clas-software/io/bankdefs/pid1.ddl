!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID1   B32   ! pid_mon sector 1 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_1    I    1   1000000 ! Number of pos Hit-Based tracks for sec=1
   2 NHBneg_1    I    1   1000000 ! Number of neg Hit-Based tracks for sec=1
   3 NTBpos_1    I    1   1000000 ! Number of pos Time-Based tracks  for sec=1
   4 NTBneg_1    I    1   1000000 ! Number of neg Time-Based tracks for sec=1
   5 chi2pos_1   F    0.    9999. ! Chi^2 for positive tracks for sec=1
   6 chi2neg_1   F    0.    9999. ! Chi^2 for negative tracks for sec=1
   7 Nunknown_1  I    1   1000000 ! Number of unknown particles for sec=1
   8 Ngamma_1    I    1   1000000 ! Number of photons for sec=1
   9 Nelec_1     I    1   1000000 ! Number of electrons for sec=1
  10 Npiplus_1   I    1   1000000 ! Number of pi+ for sec=1
  11 Npiminus_1  I    1   1000000 ! Number of pi- for sec=1
  12 Nprot_1     I    1   1000000 ! Number of proton for sec=1
  13 Nkplus_1    I    1   1000000 ! Number of K+ for sec=1
  14 Nkminus_1   I    1   1000000 ! Number of K- for sec=1
  15 Ndeuteron_1 I    1   1000000 ! Number of deuterons for sec=1
  16 Nneutron_1  I    1   1000000 ! Number of neutrons for sec=1 
!
 END TABLE
