!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID6   B32   ! pid_mon sector 6 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_6    I    1   1000000 ! Number of pos Hit-Based tracks for sec=6
   2 NHBneg_6    I    1   1000000 ! Number of neg Hit-Based tracks for sec=6
   3 NTBpos_6    I    1   1000000 ! Number of pos Time-Based tracks  for sec=6
   4 NTBneg_6    I    1   1000000 ! Number of neg Time-Based tracks for sec=6
   5 chi2pos_6   F    0.    9999. ! Chi^2 for positive tracks for sec=6
   6 chi2neg_6   F    0.    9999. ! Chi^2 for negative tracks for sec=6
   7 Nunknown_6  I    1   1000000 ! Number of unknown particles for sec=6
   8 Ngamma_6    I    1   1000000 ! Number of photons for sec=6
   9 Nelec_6     I    1   1000000 ! Number of electrons for sec=6
  10 Npiplus_6   I    1   1000000 ! Number of pi+ for sec=6
  11 Npiminus_6  I    1   1000000 ! Number of pi- for sec=6
  12 Nprot_6     I    1   1000000 ! Number of proton for sec=6
  13 Nkplus_6    I    1   1000000 ! Number of K+ for sec=6
  14 Nkminus_6   I    1   1000000 ! Number of K- for sec=6
  15 Ndeuteron_6 I    1   1000000 ! Number of deuterons for sec=6
  16 Nneutron_6  I    1   1000000 ! Number of neutrons for sec=6 
!
 END TABLE
