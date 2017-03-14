C-
      Integer Maxparts_Ichb
      parameter(maxparts_ichb=20)
      INTEGER ic_part                ! Number of IC particles 
      REAL et(maxparts_ichb)          ! Total deposited energy (GeV)
      REAL egl(maxparts_ichb)         ! energy of the lead module (GeV)
      REAL time(maxparts_ichb)        ! Time
      REAL time_next(maxparts_ichb)   ! Time of the next closest hit 
      REAL ich_X(maxparts_ichb)       ! X coordinate of the hit
      REAL ich_Y(maxparts_ichb)       ! Y coordinate of the hit
      REAL ich_Z(maxparts_ichb)       ! Z coordinate of the hit
      REAL ich_Xgl(maxparts_ichb)     ! X coordinate of the lead module
      REAL ich_Ygl(maxparts_ichb)     ! Y coordinate of the lead module
      REAL ich_Xwid(maxparts_ichb)     ! Z coordinate of the lead module
      REAL ich_Ywid(maxparts_ichb)     ! Second moment of shower transvers distr.
      REAL ich_XM3(maxparts_ichb)     ! Third moment of shower transvers distr.
      REAL ich_YM3(maxparts_ichb)     ! Forth moment of shower transvers distr.
      INTEGER IC_stat(maxparts_ichb)  ! IC hit status, number of modules
      COMMON/ichb_nt/ic_part,et,egl,time,time_next,ich_x,ich_y,ich_z
     +,ich_xgl,ich_ygl,ich_xwid,ich_ywid,ich_xm3,ich_ym3,ic_stat
