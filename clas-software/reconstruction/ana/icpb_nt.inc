C-
      Integer Maxparticles
      parameter(maxparticles=30)
        INTEGER icpart              ! Nuber of IC particles 
        REAL etc(Maxparticles)      ! Total deposited energy (GeV)
        REAL ecc(Maxparticles)     ! energy of the lead module (GeV)
        REAL tc(Maxparticles)      ! Time
        REAL tn(Maxparticles)      ! Time-next
        REAL Xc(Maxparticles)     ! X coordinate of the hit
        REAL Yc(Maxparticles)     ! Y coordinate of the hit
        REAL Zc(Maxparticles)     ! Z coordinate of the hit
        REAL M2c(Maxparticles)     ! Second moment of shower transvers distr.
        REAL M3c(Maxparticles)     ! Third moment of shower transvers distr.
        INTEGER statc(Maxparticles)  ! IC hit status, number of modules
      COMMON/icpb_nt/icpart,etc,ecc,tc,tn,xc,yc,zc,m2c,m3c,statc
