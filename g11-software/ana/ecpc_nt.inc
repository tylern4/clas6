c      include file ECPC_NT
c


      INTEGER ecpc_nhit         ! Number of entries in ntuple
      INTEGER ecpc_hit(256)     ! ECPC bank number sector*100+layer*10+hit
      INTEGER ecpc_id(256)      ! layer(1-6)*100 + stripID
      INTEGER ecpc_tdc(256)     ! Raw tdc
      INTEGER ecpc_adc(256)     ! Raw adc - pedestal
     
      COMMON/ecpc_nt/ecpc_nhit,ecpc_hit,ecpc_id,ecpc_tdc,ecpc_adc
