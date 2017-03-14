!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID3   B32   ! pid_mon sector 3 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_3    I    1   1000000 ! Number of pos Hit-Based tracks for sec=3
   2 NHBneg_3    I    1   1000000 ! Number of neg Hit-Based tracks for sec=3
   3 NTBpos_3    I    1   1000000 ! Number of pos Time-Based tracks  for sec=3
   4 NTBneg_3    I    1   1000000 ! Number of neg Time-Based tracks for sec=3
   5 chi2pos_3   F    0.    9999. ! Chi^2 for positive tracks for sec=3
   6 chi2neg_3   F    0.    9999. ! Chi^2 for negative tracks for sec=3
   7 Nunknown_3  I    1   1000000 ! Number of unknown particles for sec=3
   8 Ngamma_3    I    1   1000000 ! Number of photons for sec=3
   9 Nelec_3     I    1   1000000 ! Number of electrons for sec=3
  10 Npiplus_3   I    1   1000000 ! Number of pi+ for sec=3
  11 Npiminus_3  I    1   1000000 ! Number of pi- for sec=3
  12 Nprot_3     I    1   1000000 ! Number of proton for sec=3
  13 Nkplus_3    I    1   1000000 ! Number of K+ for sec=3
  14 Nkminus_3   I    1   1000000 ! Number of K- for sec=3
  15 Ndeuteron_3 I    1   1000000 ! Number of deuterons for sec=3
  16 Nneutron_3  I    1   1000000 ! Number of neutrons for sec=3
!
 END TABLE
