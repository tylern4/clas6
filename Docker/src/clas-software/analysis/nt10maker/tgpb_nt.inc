      INTEGER Maxphotons
      PARAMETER (Maxphotons=100)

      integer nphotons                 ! number of reconstructed photons
                                       ! in tagger
      integer tgpb_pointer(Maxphotons) ! pointer to TAGR bank
      real tgpb_Time(Maxphotons)       ! starttime_TAG at interaction
                                       ! point
      real tgpb_Energy(Maxphotons)     ! photon energy (GeV)
      real tgpb_dt(Maxphotons)         ! starttime_ST - starttime_TAG
                                       ! (ns)
      COMMON/tgpb_nt/nphotons, tgpb_pointer, tgpb_Time,
     $     tgpb_Energy, tgpb_dt
