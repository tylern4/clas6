      character*10 cl_target
      REAL cl_beam_energy,cl_zpos,cl_zwidth,cl_zmin
      REAL  cl_emin,cl_emax,cl_tmin,cl_tmax
      REAL  cl_xmin,cl_xmax,cl_qmin,cl_qmax,cl_wmin,cl_wmax
      integer cl_verblev,cl_rand,cl_nprint,cl_mstu,cl_pid
      integer cl_triggers,cl_pol,cl_mstp,cl_vmstp
      integer  cl_smear
      integer  cl_ht,cl_outform
      real  cl_sma,cl_smb,cl_smc,cl_smd
      logical clasdisOK,ntOK,cl_radgen,datfileOK
      logical acce16,acceg1
      character*70 bosout,datfilename,cl_path
      common /OUT_NAMES/ bosout
      common /claspepsi1/ cl_verblev,cl_nprint,cl_rand,cl_mstu
     6                   ,cl_mstp,cl_vmstp,cl_ht
      common /claspepsi2/ cl_target
      common /claspepsi3/ cl_triggers,cl_pid,cl_beam_energy,cl_pol
      common /claspepsi4/ cl_zpos,cl_zwidth,cl_zmin
      common /claspepsi5/ cl_emin,cl_emax,cl_tmin,cl_tmax
      common /claspepsi6/ cl_smear,cl_sma,cl_smb,cl_smc,cl_smd
      common /claspepsi6/ cl_xmin,cl_xmax,cl_qmin,cl_qmax,cl_wmin,cl_wmax
      common /clasdiscont1/ clasdisOK,ntOK,cl_radgen,acce16,acceg1
      common /clasdisfile/ datfileOK,datfilename,cl_path,cl_outform
