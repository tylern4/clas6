      INTEGER Maxphot_tagr
      PARAMETER (Maxphot_tagr=40)

      integer nphot_tagr               
      real tagr_E_gamma(Maxphot_tagr) 
      real tagr_ttag(Maxphot_tagr)     
      real tagr_tpho(Maxphot_tagr)
      integer tagr_stat(Maxphot_tagr)
      integer tagr_Tid(Maxphot_tagr)
      integer tagr_Eid(Maxphot_tagr)

      COMMON/tagr_nt/nphot_tagr, tagr_E_gamma,
     $     tagr_ttag, tagr_tpho, tagr_stat, tagr_tid, tagr_eid
