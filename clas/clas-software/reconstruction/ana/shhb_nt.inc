C-
      Integer Maxparts_shhb
      parameter(maxparts_shhb=72)
      INTEGER shh_part                  ! Number of SH particles 
      INTEGER shh_id(maxparts_shhb)     ! Channel number
      REAL shh_x(maxparts_shhb)         ! X coordinate of the hit
      REAL shh_y(maxparts_shhb)         ! Y coordinate of the hit
      REAL shh_z(maxparts_shhb)         ! Z coordinate of the hit
      REAL shh_nphe(maxparts_shhb)      ! number of photoelectrons
      REAL shh_time(maxparts_shhb)      ! Time     
      INTEGER shh_stat(maxparts_shhb)   ! SH hit status
      COMMON/shhb_nt/shh_part,shh_id,shh_x,shh_y,shh_z,
     +shh_nphe,shh_time,shh_stat

