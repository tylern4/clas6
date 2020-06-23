!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID4   B32   ! pid_mon sector 4 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_4    I    1   1000000 ! Number of pos Hit-Based tracks for sec=4
   2 NHBneg_4    I    1   1000000 ! Number of neg Hit-Based tracks for sec=4
   3 NTBpos_4    I    1   1000000 ! Number of pos Time-Based tracks  for sec=4
   4 NTBneg_4    I    1   1000000 ! Number of neg Time-Based tracks for sec=4
   5 chi2pos_4   F    0.    9999. ! Chi^2 for positive tracks for sec=4
   6 chi2neg_4   F    0.    9999. ! Chi^2 for negative tracks for sec=4
   7 Nunknown_4  I    1   1000000 ! Number of unknown particles for sec=4
   8 Ngamma_4    I    1   1000000 ! Number of photons for sec=4
   9 Nelec_4     I    1   1000000 ! Number of electrons for sec=4
  10 Npiplus_4   I    1   1000000 ! Number of pi+ for sec=4
  11 Npiminus_4  I    1   1000000 ! Number of pi- for sec=4
  12 Nprot_4     I    1   1000000 ! Number of proton for sec=4
  13 Nkplus_4    I    1   1000000 ! Number of K+ for sec=4
  14 Nkminus_4   I    1   1000000 ! Number of K- for sec=4
  15 Ndeuteron_4 I    1   1000000 ! Number of deuterons for sec=4
  16 Nneutron_4  I    1   1000000 ! Number of neutrons for sec=4   
!
 END TABLE
