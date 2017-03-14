!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID2   B32   ! pid_mon sector 2 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_2    I    1   1000000 ! Number of pos Hit-Based tracks for sec=2
   2 NHBneg_2    I    1   1000000 ! Number of neg Hit-Based tracks for sec=2
   3 NTBpos_2    I    1   1000000 ! Number of pos Time-Based tracks  for sec=2
   4 NTBneg_2    I    1   1000000 ! Number of neg Time-Based tracks for sec=2
   5 chi2pos_2   F    0.    9999. ! Chi^2 for positive tracks for sec=2
   6 chi2neg_2   F    0.    9999. ! Chi^2 for negative tracks for sec=2
   7 Nunknown_2  I    1   1000000 ! Number of unknown particles for sec=2
   8 Ngamma_2    I    1   1000000 ! Number of photons for sec=2
   9 Nelec_2     I    1   1000000 ! Number of electrons for sec=2
  10 Npiplus_2   I    1   1000000 ! Number of pi+ for sec=2
  11 Npiminus_2  I    1   1000000 ! Number of pi- for sec=2
  12 Nprot_2     I    1   1000000 ! Number of proton for sec=2
  13 Nkplus_2    I    1   1000000 ! Number of K+ for sec=2
  14 Nkminus_2   I    1   1000000 ! Number of K- for sec=2
  15 Ndeuteron_2 I    1   1000000 ! Number of deuterons for sec=2
  16 Nneutron_2  I    1   1000000 ! Number of neutrons for sec=2
!
 END TABLE
