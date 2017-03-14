c Include file for csql bank.
      real q_t_l, q_t, t_l_t,I_fc,totcpu
      INTEGER nelect(6),ntbttrk,nhbttrk,nelectp,nelectn
      INTEGER nprot,npip,npim,ndeut,nkaonp, nkaonm, nphoton,ngamma
      INTEGER nel,npos,nneg,nph
      INTEGER npipsect(6), npimsect(6), nkoanpsect(6), nkoanmsect(6)
      INTEGER nprotsect(6), nphotsect(6), nhbsect(6), ntbsect(6)
      INTEGER ng_norm
      REAL    nphot1(61),err_nphot1(61)
      REAL    nphot2(767),err_nphot2(767)
      REAL    nphot3(225),err_nphot3(225)
      REAL    nphot4(90),err_nphot4(90)
      REAL    trip_flag(100),trip_min(100),trip_max(100),ntrip

c
      common/csql/q_t_l, q_t, t_l_t,I_fc,totcpu,nelect
     +,ntbttrk,nhbttrk,nelectp,nelectn,nprot,npip,npim,ndeut,nkaonp,nkaonm 
     +,nphoton,nel,npos,nneg,nph,ngamma,npipsect,npimsect
     +,nkoanpsect,nkoanmsect,nphotsect,nhbsect,ntbsect
     +,nprotsect
     +,ng_norm,nphot1,err_nphot1,nphot2,err_nphot2
     +,nphot3,err_nphot3,nphot4,err_nphot4
     +,trip_flag,trip_min,trip_max,ntrip
