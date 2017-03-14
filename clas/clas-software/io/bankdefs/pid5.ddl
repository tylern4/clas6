!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID5   B32   ! pid_mon sector 5 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_5    I    1   1000000 ! Number of pos Hit-Based tracks for sec=5
   2 NHBneg_5    I    1   1000000 ! Number of neg Hit-Based tracks for sec=5
   3 NTBpos_5    I    1   1000000 ! Number of pos Time-Based tracks  for sec=5
   4 NTBneg_5    I    1   1000000 ! Number of neg Time-Based tracks for sec=5
   5 chi2pos_5   F    0.    9999. ! Chi^2 for positive tracks for sec=5
   6 chi2neg_5   F    0.    9999. ! Chi^2 for negative tracks for sec=5
   7 Nunknown_5  I    1   1000000 ! Number of unknown particles for sec=5
   8 Ngamma_5    I    1   1000000 ! Number of photons for sec=5
   9 Nelec_5     I    1   1000000 ! Number of electrons for sec=5
  10 Npiplus_5   I    1   1000000 ! Number of pi+ for sec=5
  11 Npiminus_5  I    1   1000000 ! Number of pi- for sec=5
  12 Nprot_5     I    1   1000000 ! Number of proton for sec=5
  13 Nkplus_5    I    1   1000000 ! Number of K+ for sec=5
  14 Nkminus_5   I    1   1000000 ! Number of K- for sec=5
  15 Ndeuteron_5 I    1   1000000 ! Number of deuterons for sec=5
  16 Nneutron_5  I    1   1000000 ! Number of neutrons for sec=5 
!
 END TABLE
