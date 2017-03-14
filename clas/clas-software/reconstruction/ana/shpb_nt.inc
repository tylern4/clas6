C-
      Integer Maxparts_shpb
      parameter(maxparts_shpb=54)
      INTEGER shpart                  ! Number of SH particles 
      INTEGER shid(maxparts_shpb)     ! Channel number
      REAL shx(maxparts_shpb)         ! X coordinate of the hit
      REAL shy(maxparts_shpb)         ! Y coordinate of the hit
      REAL shz(maxparts_shpb)         ! Z coordinate of the hit
      REAL shnphe(maxparts_shpb)      ! number of photoelectrons
      REAL shtime(maxparts_shpb)      ! Time     
      INTEGER shstat(maxparts_shpb)   ! SH hit status
      COMMON/shpb_nt/shpart,shid,shx,shy,shz,
     +shnphe,shtime,shstat

